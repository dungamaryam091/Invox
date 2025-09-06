;; Invoice Template System
;; Enables users to create reusable invoice templates for streamlined invoice generation

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u301))
(define-constant ERR_INVALID_AMOUNT (err u302))
(define-constant ERR_TEMPLATE_INACTIVE (err u303))
(define-constant ERR_INVALID_CATEGORY (err u304))
(define-constant ERR_TEMPLATE_LIMIT_EXCEEDED (err u305))

;; Data variables
(define-data-var next-template-id uint u1)
(define-data-var max-templates-per-user uint u50)
(define-data-var total-templates uint u0)

;; Template data structure
(define-map invoice-templates
  uint
  {
    creator: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    amount: uint,
    default-due-days: uint,
    category: (string-ascii 32),
    is-active: bool,
    created-at: uint,
    last-used: uint,
    usage-count: uint,
    payment-terms: (string-ascii 200),
    line-items: (list 5 {item: (string-ascii 100), amount: uint})
  }
)

;; Template usage analytics
(define-map template-stats
  uint
  {
    total-invoices-generated: uint,
    total-amount-invoiced: uint,
    success-rate: uint,
    last-generation: uint,
    average-payment-time: uint
  }
)

;; User template counts
(define-map user-template-count
  principal
  uint
)

;; Template categories for organization
(define-map template-categories
  (string-ascii 32)
  {
    template-count: uint,
    total-usage: uint,
    description: (string-ascii 100)
  }
)

;; Create a new invoice template
(define-public (create-template
  (name (string-ascii 64))
  (description (string-ascii 256))
  (amount uint)
  (default-due-days uint)
  (category (string-ascii 32))
  (payment-terms (string-ascii 200))
  (line-items (list 5 {item: (string-ascii 100), amount: uint})))
  (let (
    (template-id (var-get next-template-id))
    (current-block stacks-block-height)
    (user-templates (default-to u0 (map-get? user-template-count tx-sender)))
  )
    ;; Validations
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (< user-templates (var-get max-templates-per-user)) ERR_TEMPLATE_LIMIT_EXCEEDED)
    (asserts! (> (len category) u0) ERR_INVALID_CATEGORY)
    
    ;; Create template
    (map-set invoice-templates template-id
      {
        creator: tx-sender,
        name: name,
        description: description,
        amount: amount,
        default-due-days: default-due-days,
        category: category,
        is-active: true,
        created-at: current-block,
        last-used: u0,
        usage-count: u0,
        payment-terms: payment-terms,
        line-items: line-items
      })
    
    ;; Initialize template stats
    (map-set template-stats template-id
      {
        total-invoices-generated: u0,
        total-amount-invoiced: u0,
        success-rate: u0,
        last-generation: u0,
        average-payment-time: u0
      })
    
    ;; Update counters
    (var-set next-template-id (+ template-id u1))
    (var-set total-templates (+ (var-get total-templates) u1))
    (map-set user-template-count tx-sender (+ user-templates u1))
    
    ;; Update category stats
    (let ((cat-stats (default-to 
                       {template-count: u0, total-usage: u0, description: ""}
                       (map-get? template-categories category))))
      (map-set template-categories category
        (merge cat-stats {template-count: (+ (get template-count cat-stats) u1)}))
    )
    
    (ok template-id)
  ))

;; Generate invoice from template
(define-public (generate-invoice-from-template
  (template-id uint)
  (payer principal)
  (custom-due-days (optional uint)))
  (let (
    (template (unwrap! (map-get? invoice-templates template-id) ERR_TEMPLATE_NOT_FOUND))
    (current-block stacks-block-height)
    (due-days (default-to (get default-due-days template) custom-due-days))
    (due-date (+ current-block due-days))
    (stats (default-to 
             {total-invoices-generated: u0, total-amount-invoiced: u0, 
              success-rate: u0, last-generation: u0, average-payment-time: u0}
             (map-get? template-stats template-id)))
  )
    ;; Validations
    (asserts! (is-eq (get creator template) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active template) ERR_TEMPLATE_INACTIVE)
    
    ;; Generate invoice using main contract (simplified call)
    ;; In production, this would call the main Invox contract
    (let ((invoice-result (try! (contract-call? .Invox create-invoice 
                                  payer 
                                  (get amount template) 
                                  due-date 
                                  (get description template)))))
      
      ;; Update template usage
      (map-set invoice-templates template-id
        (merge template {
          last-used: current-block,
          usage-count: (+ (get usage-count template) u1)
        }))
      
      ;; Update template stats
      (map-set template-stats template-id
        (merge stats {
          total-invoices-generated: (+ (get total-invoices-generated stats) u1),
          total-amount-invoiced: (+ (get total-amount-invoiced stats) (get amount template)),
          last-generation: current-block
        }))
      
      ;; Update category usage
      (let ((cat-stats (default-to 
                         {template-count: u0, total-usage: u0, description: ""}
                         (map-get? template-categories (get category template)))))
        (map-set template-categories (get category template)
          (merge cat-stats {total-usage: (+ (get total-usage cat-stats) u1)}))
      )
      
      (ok {template-id: template-id, invoice-id: invoice-result})
    )
  ))

;; Update template details
(define-public (update-template
  (template-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (amount uint)
  (default-due-days uint)
  (payment-terms (string-ascii 200)))
  (let (
    (template (unwrap! (map-get? invoice-templates template-id) ERR_TEMPLATE_NOT_FOUND))
  )
    (asserts! (is-eq (get creator template) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set invoice-templates template-id
      (merge template {
        name: name,
        description: description,
        amount: amount,
        default-due-days: default-due-days,
        payment-terms: payment-terms
      }))
    
    (ok true)
  ))

;; Toggle template active status
(define-public (toggle-template-status (template-id uint))
  (let (
    (template (unwrap! (map-get? invoice-templates template-id) ERR_TEMPLATE_NOT_FOUND))
  )
    (asserts! (is-eq (get creator template) tx-sender) ERR_NOT_AUTHORIZED)
    
    (map-set invoice-templates template-id
      (merge template {is-active: (not (get is-active template))}))
    
    (ok (not (get is-active template)))
  ))

;; Create or update template category
(define-public (manage-category
  (category-name (string-ascii 32))
  (description (string-ascii 100)))
  (let (
    (existing-cat (map-get? template-categories category-name))
  )
    (map-set template-categories category-name
      {
        template-count: (match existing-cat cat (get template-count cat) u0),
        total-usage: (match existing-cat cat (get total-usage cat) u0),
        description: description
      })
    
    (ok true)
  ))

;; Read-only functions

;; Get template details
(define-read-only (get-template (template-id uint))
  (map-get? invoice-templates template-id))

;; Get template statistics
(define-read-only (get-template-stats (template-id uint))
  (map-get? template-stats template-id))

;; Get user's template count
(define-read-only (get-user-template-count (user principal))
  (default-to u0 (map-get? user-template-count user)))

;; Get category information
(define-read-only (get-category-info (category (string-ascii 32)))
  (map-get? template-categories category))

;; Get total system templates
(define-read-only (get-total-templates)
  (var-get total-templates))

;; Check if template is active and usable
(define-read-only (is-template-active (template-id uint))
  (match (map-get? invoice-templates template-id)
    template (and (get is-active template) (is-eq (get creator template) tx-sender))
    false))

;; Get template usage efficiency
(define-read-only (get-template-efficiency (template-id uint))
  (match (map-get? template-stats template-id)
    stats (let ((generated (get total-invoices-generated stats)))
            (if (> generated u0)
              {
                total-generated: generated,
                average-amount: (/ (get total-amount-invoiced stats) generated),
                efficiency-score: (if (> (get success-rate stats) u0) 
                                    (/ (* (get success-rate stats) generated) u100) 
                                    u0)
              }
              {total-generated: u0, average-amount: u0, efficiency-score: u0}))
    {total-generated: u0, average-amount: u0, efficiency-score: u0}))

;; Get popular templates by usage
(define-read-only (get-template-popularity (template-id uint))
  (match (map-get? invoice-templates template-id)
    template (let ((usage (get usage-count template)))
               {
                 template-id: template-id,
                 name: (get name template),
                 category: (get category template),
                 usage-count: usage,
                 last-used: (get last-used template),
                 popularity-score: (if (> usage u0) (* usage u10) u0)
               })
    {template-id: u0, name: "", category: "", usage-count: u0, last-used: u0, popularity-score: u0}))

;; Calculate template ROI based on usage
(define-read-only (get-template-roi (template-id uint))
  (match (map-get? template-stats template-id)
    stats (let ((generated (get total-invoices-generated stats))
                (total-amount (get total-amount-invoiced stats)))
            (if (> generated u0)
              {
                invoices-generated: generated,
                total-value: total-amount,
                average-value: (/ total-amount generated),
                time-saved: (* generated u10) ;; Assume 10 time units saved per template use
              }
              {invoices-generated: u0, total-value: u0, average-value: u0, time-saved: u0}))
    {invoices-generated: u0, total-value: u0, average-value: u0, time-saved: u0}))
