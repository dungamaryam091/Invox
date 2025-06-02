
(define-non-fungible-token invox uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-payment-failed (err u103))
(define-constant err-invalid-status (err u104))

(define-data-var last-token-id uint u0)

(define-map invoices
    uint 
    {
        creator: principal,
        payer: principal,
        amount: uint,
        due-date: uint,
        status: (string-ascii 20),
        description: (string-ascii 256)
    }
)

(define-map invoice-status
    uint 
    {
        paid: bool,
        payment-date: uint,
        payment-tx: (optional (string-ascii 64))
    }
)

(define-public (create-invoice (payer principal) (amount uint) (due-date uint) (description (string-ascii 256)))
    (let
        (
            (new-id (+ (var-get last-token-id) u1))
        )
        (try! (nft-mint? invox new-id tx-sender))
        (var-set last-token-id new-id)
        (map-set invoices new-id {
            creator: tx-sender,
            payer: payer,
            amount: amount,
            due-date: due-date,
            status: "PENDING",
            description: description
        })
        (map-set invoice-status new-id {
            paid: false,
            payment-date: u0,
            payment-tx: none
        })
        (ok new-id)
    )
)

(define-public (pay-invoice (invoice-id uint) (payment-tx (string-ascii 64)))
    (let
        (
            (invoice (unwrap! (map-get? invoices invoice-id) err-not-found))
            (status (unwrap! (map-get? invoice-status invoice-id) err-not-found))
        )
        (asserts! (is-eq (get payer invoice) tx-sender) err-owner-only)
        (asserts! (is-eq (get status invoice) "PENDING") err-invalid-status)
        (try! (stx-transfer? (get amount invoice) tx-sender (get creator invoice)))
        (map-set invoices invoice-id (merge invoice { status: "PAID" }))
        (map-set invoice-status invoice-id {
            paid: true,
            payment-date: stacks-block-height,
            payment-tx: (some payment-tx)
        })
        (ok true)
    )
)

(define-public (cancel-invoice (invoice-id uint))
    (let
        (
            (invoice (unwrap! (map-get? invoices invoice-id) err-not-found))
        )
        (asserts! (is-eq (get creator invoice) tx-sender) err-owner-only)
        (asserts! (is-eq (get status invoice) "PENDING") err-invalid-status)
        (map-set invoices invoice-id (merge invoice { status: "CANCELLED" }))
        (ok true)
    )
)

(define-read-only (get-invoice (invoice-id uint))
    (ok (unwrap! (map-get? invoices invoice-id) err-not-found))
)

(define-read-only (get-invoice-status (invoice-id uint))
    (ok (unwrap! (map-get? invoice-status invoice-id) err-not-found))
)


(define-private (created-by-principal (entry {id: uint, value: {creator: principal, payer: principal, amount: uint, due-date: uint, status: (string-ascii 20), description: (string-ascii 256)}}))
    (is-eq (get creator (get value entry)) tx-sender)
)

(define-private (payable-by-principal (entry {id: uint, value: {creator: principal, payer: principal, amount: uint, due-date: uint, status: (string-ascii 20), description: (string-ascii 256)}}))
    (is-eq (get payer (get value entry)) tx-sender)
)


(define-private (add-entry (id uint) (entries (list 100 {id: uint, value: {creator: principal, payer: principal, amount: uint, due-date: uint, status: (string-ascii 20), description: (string-ascii 256)}})))
    (match (map-get? invoices id)
        value (append entries {id: id, value: value})
        entries
    )
)