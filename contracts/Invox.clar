
(define-non-fungible-token invox uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-payment-failed (err u103))
(define-constant err-invalid-status (err u104))
(define-constant err-subscription-not-found (err u105))
(define-constant err-subscription-already-cancelled (err u106))
(define-constant err-subscription-not-active (err u107))
(define-constant err-payment-not-due (err u108))
(define-constant err-invalid-interval (err u109))
(define-constant err-escrow-not-found (err u110))
(define-constant err-escrow-already-funded (err u111))
(define-constant err-escrow-not-funded (err u112))
(define-constant err-escrow-already-completed (err u113))
(define-constant err-not-authorized (err u114))
(define-constant err-dispute-already-exists (err u115))
(define-constant err-dispute-not-found (err u116))
(define-constant err-invalid-arbitrator (err u117))
(define-constant err-escrow-under-dispute (err u118))

(define-data-var last-token-id uint u0)
(define-data-var last-subscription-id uint u0)
(define-data-var last-escrow-id uint u0)
(define-data-var last-dispute-id uint u0)

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

(define-map subscriptions
    uint
    {
        creator: principal,
        subscriber: principal,
        amount: uint,
        interval-blocks: uint,
        description: (string-ascii 256),
        status: (string-ascii 20),
        created-at: uint,
        next-payment-due: uint,
        total-payments: uint,
        max-payments: (optional uint)
    }
)

(define-map subscription-payments
    uint
    {
        subscription-id: uint,
        payment-number: uint,
        amount: uint,
        payment-date: uint,
        payment-tx: (string-ascii 64),
        auto-generated: bool
    }
)

(define-map escrows
    uint
    {
        invoice-id: uint,
        payer: principal,
        payee: principal,
        amount: uint,
        status: (string-ascii 20),
        created-at: uint,
        funded-at: (optional uint),
        completed-at: (optional uint),
        arbitrator: (optional principal),
        description: (string-ascii 256)
    }
)

(define-map disputes
    uint
    {
        escrow-id: uint,
        initiator: principal,
        reason: (string-ascii 512),
        status: (string-ascii 20),
        created-at: uint,
        resolved-at: (optional uint),
        resolution: (optional (string-ascii 512)),
        winner: (optional principal)
    }
)

(define-map arbitrators
    principal
    {
        active: bool,
        total-cases: uint,
        reputation-score: uint
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

(define-public (create-subscription (subscriber principal) (amount uint) (interval-blocks uint) (description (string-ascii 256)) (max-payments (optional uint)))
    (begin
        (asserts! (> interval-blocks u0) err-invalid-interval)
        (asserts! (> amount u0) err-invalid-interval)
        (let
            (
                (new-id (+ (var-get last-subscription-id) u1))
                (current-block stacks-block-height)
            )
            (var-set last-subscription-id new-id)
            (map-set subscriptions new-id {
                creator: tx-sender,
                subscriber: subscriber,
                amount: amount,
                interval-blocks: interval-blocks,
                description: description,
                status: "ACTIVE",
                created-at: current-block,
                next-payment-due: (+ current-block interval-blocks),
                total-payments: u0,
                max-payments: max-payments
            })
            (ok new-id)
        )
    )
)

(define-public (pay-subscription (subscription-id uint) (payment-tx (string-ascii 64)))
    (let
        (
            (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
            (current-block stacks-block-height)
            (payment-number (+ (get total-payments subscription) u1))
        )
        (asserts! (is-eq (get subscriber subscription) tx-sender) err-owner-only)
        (asserts! (is-eq (get status subscription) "ACTIVE") err-subscription-not-active)
        (asserts! (<= (get next-payment-due subscription) current-block) err-payment-not-due)
        (try! (stx-transfer? (get amount subscription) tx-sender (get creator subscription)))
        (let
            (
                (new-next-due (+ current-block (get interval-blocks subscription)))
                (new-total-payments payment-number)
                (should-cancel (match (get max-payments subscription)
                    max-pay (>= new-total-payments max-pay)
                    false))
                (new-status (if should-cancel "COMPLETED" "ACTIVE"))
            )
            (map-set subscriptions subscription-id (merge subscription {
                next-payment-due: new-next-due,
                total-payments: new-total-payments,
                status: new-status
            }))
            (map-set subscription-payments (+ (* subscription-id u1000) payment-number) {
                subscription-id: subscription-id,
                payment-number: payment-number,
                amount: (get amount subscription),
                payment-date: current-block,
                payment-tx: payment-tx,
                auto-generated: false
            })
            (ok payment-number)
        )
    )
)

(define-public (cancel-subscription (subscription-id uint))
    (let
        (
            (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
        )
        (asserts! (is-eq (get creator subscription) tx-sender) err-owner-only)
        (asserts! (not (is-eq (get status subscription) "CANCELLED")) err-subscription-already-cancelled)
        (map-set subscriptions subscription-id (merge subscription { status: "CANCELLED" }))
        (ok true)
    )
)

(define-public (process-due-subscription (subscription-id uint))
    (let
        (
            (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
            (current-block stacks-block-height)
            (payment-number (+ (get total-payments subscription) u1))
        )
        (asserts! (is-eq (get status subscription) "ACTIVE") err-subscription-not-active)
        (asserts! (<= (get next-payment-due subscription) current-block) err-payment-not-due)
        (let
            (
                (new-next-due (+ current-block (get interval-blocks subscription)))
                (new-total-payments payment-number)
                (should-cancel (match (get max-payments subscription)
                    max-pay (>= new-total-payments max-pay)
                    false))
                (new-status (if should-cancel "COMPLETED" "OVERDUE"))
            )
            (map-set subscriptions subscription-id (merge subscription {
                next-payment-due: new-next-due,
                total-payments: new-total-payments,
                status: new-status
            }))
            (ok payment-number)
        )
    )
)

(define-public (update-subscription-interval (subscription-id uint) (new-interval-blocks uint))
    (let
        (
            (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
        )
        (asserts! (is-eq (get creator subscription) tx-sender) err-owner-only)
        (asserts! (is-eq (get status subscription) "ACTIVE") err-subscription-not-active)
        (asserts! (> new-interval-blocks u0) err-invalid-interval)
        (let
            (
                (current-block stacks-block-height)
                (new-next-due (+ current-block new-interval-blocks))
            )
            (map-set subscriptions subscription-id (merge subscription {
                interval-blocks: new-interval-blocks,
                next-payment-due: new-next-due
            }))
            (ok true)
        )
    )
)

(define-read-only (get-subscription (subscription-id uint))
    (ok (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
)

(define-read-only (get-subscription-payment (subscription-id uint) (payment-number uint))
    (ok (unwrap! (map-get? subscription-payments (+ (* subscription-id u1000) payment-number)) err-not-found))
)

(define-read-only (is-subscription-payment-due (subscription-id uint))
    (match (map-get? subscriptions subscription-id)
        subscription
        (ok (and 
            (is-eq (get status subscription) "ACTIVE")
            (<= (get next-payment-due subscription) stacks-block-height)
        ))
        err-subscription-not-found
    )
)

(define-read-only (get-subscription-revenue (subscription-id uint))
    (let
        (
            (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
        )
        (ok (* (get amount subscription) (get total-payments subscription)))
    )
)

(define-read-only (get-total-subscriptions)
    (var-get last-subscription-id)
)

(define-private (subscription-created-by-principal (entry {id: uint, value: {creator: principal, subscriber: principal, amount: uint, interval-blocks: uint, description: (string-ascii 256), status: (string-ascii 20), created-at: uint, next-payment-due: uint, total-payments: uint, max-payments: (optional uint)}}))
    (is-eq (get creator (get value entry)) tx-sender)
)

(define-private (subscription-for-principal (entry {id: uint, value: {creator: principal, subscriber: principal, amount: uint, interval-blocks: uint, description: (string-ascii 256), status: (string-ascii 20), created-at: uint, next-payment-due: uint, total-payments: uint, max-payments: (optional uint)}}))
    (is-eq (get subscriber (get value entry)) tx-sender)
)

(define-private (add-subscription-entry (id uint) (entries (list 100 {id: uint, value: {creator: principal, subscriber: principal, amount: uint, interval-blocks: uint, description: (string-ascii 256), status: (string-ascii 20), created-at: uint, next-payment-due: uint, total-payments: uint, max-payments: (optional uint)}})))
    (match (map-get? subscriptions id)
        value (append entries {id: id, value: value})
        entries
    )
)

(define-public (create-escrow (invoice-id uint) (payee principal) (arbitrator (optional principal)) (description (string-ascii 256)))
    (let
        (
            (invoice (unwrap! (map-get? invoices invoice-id) err-not-found))
            (new-escrow-id (+ (var-get last-escrow-id) u1))
        )
        (asserts! (is-eq (get payer invoice) tx-sender) err-not-authorized)
        (asserts! (is-eq (get status invoice) "PENDING") err-invalid-status)
        (var-set last-escrow-id new-escrow-id)
        (map-set escrows new-escrow-id {
            invoice-id: invoice-id,
            payer: tx-sender,
            payee: payee,
            amount: (get amount invoice),
            status: "CREATED",
            created-at: stacks-block-height,
            funded-at: none,
            completed-at: none,
            arbitrator: arbitrator,
            description: description
        })
        (ok new-escrow-id)
    )
)

(define-public (fund-escrow (escrow-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
        )
        (asserts! (is-eq (get payer escrow) tx-sender) err-not-authorized)
        (asserts! (is-eq (get status escrow) "CREATED") err-escrow-already-funded)
        (try! (stx-transfer? (get amount escrow) tx-sender (as-contract tx-sender)))
        (map-set escrows escrow-id (merge escrow {
            status: "FUNDED",
            funded-at: (some stacks-block-height)
        }))
        (ok true)
    )
)

(define-public (complete-escrow (escrow-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
        )
        (asserts! (is-eq (get payee escrow) tx-sender) err-not-authorized)
        (asserts! (is-eq (get status escrow) "FUNDED") err-escrow-not-funded)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get payee escrow))))
        (map-set escrows escrow-id (merge escrow {
            status: "COMPLETED",
            completed-at: (some stacks-block-height)
        }))
        (ok true)
    )
)

(define-public (refund-escrow (escrow-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
        )
        (asserts! (is-eq (get payer escrow) tx-sender) err-not-authorized)
        (asserts! (is-eq (get status escrow) "FUNDED") err-escrow-not-funded)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get payer escrow))))
        (map-set escrows escrow-id (merge escrow {
            status: "REFUNDED",
            completed-at: (some stacks-block-height)
        }))
        (ok true)
    )
)

(define-public (register-arbitrator)
    (begin
        (map-set arbitrators tx-sender {
            active: true,
            total-cases: u0,
            reputation-score: u100
        })
        (ok true)
    )
)

(define-public (deactivate-arbitrator)
    (let
        (
            (arbitrator (unwrap! (map-get? arbitrators tx-sender) err-invalid-arbitrator))
        )
        (map-set arbitrators tx-sender (merge arbitrator { active: false }))
        (ok true)
    )
)

(define-public (initiate-dispute (escrow-id uint) (reason (string-ascii 512)))
    (let
        (
            (escrow (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
            (new-dispute-id (+ (var-get last-dispute-id) u1))
        )
        (asserts! (or (is-eq (get payer escrow) tx-sender) (is-eq (get payee escrow) tx-sender)) err-not-authorized)
        (asserts! (is-eq (get status escrow) "FUNDED") err-escrow-not-funded)
        (var-set last-dispute-id new-dispute-id)
        (map-set disputes new-dispute-id {
            escrow-id: escrow-id,
            initiator: tx-sender,
            reason: reason,
            status: "OPEN",
            created-at: stacks-block-height,
            resolved-at: none,
            resolution: none,
            winner: none
        })
        (map-set escrows escrow-id (merge escrow { status: "DISPUTED" }))
        (ok new-dispute-id)
    )
)

(define-public (resolve-dispute (dispute-id uint) (winner principal) (resolution (string-ascii 512)))
    (let
        (
            (dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found))
            (escrow (unwrap! (map-get? escrows (get escrow-id dispute)) err-escrow-not-found))
            (arbitrator-data (unwrap! (map-get? arbitrators tx-sender) err-invalid-arbitrator))
        )
        (asserts! (match (get arbitrator escrow) arb (is-eq arb tx-sender) false) err-not-authorized)
        (asserts! (get active arbitrator-data) err-invalid-arbitrator)
        (asserts! (is-eq (get status dispute) "OPEN") err-dispute-not-found)
        (asserts! (or (is-eq winner (get payer escrow)) (is-eq winner (get payee escrow))) err-not-authorized)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender winner)))
        (map-set disputes dispute-id (merge dispute {
            status: "RESOLVED",
            resolved-at: (some stacks-block-height),
            resolution: (some resolution),
            winner: (some winner)
        }))
        (map-set escrows (get escrow-id dispute) (merge escrow {
            status: "RESOLVED",
            completed-at: (some stacks-block-height)
        }))
        (map-set arbitrators tx-sender (merge arbitrator-data {
            total-cases: (+ (get total-cases arbitrator-data) u1)
        }))
        (ok true)
    )
)

(define-read-only (get-escrow (escrow-id uint))
    (ok (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (unwrap! (map-get? disputes dispute-id) err-dispute-not-found))
)

(define-read-only (get-arbitrator (arbitrator-principal principal))
    (ok (unwrap! (map-get? arbitrators arbitrator-principal) err-invalid-arbitrator))
)

(define-read-only (get-escrow-balance (escrow-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
        )
        (if (is-eq (get status escrow) "FUNDED")
            (ok (get amount escrow))
            (ok u0)
        )
    )
)

(define-read-only (is-escrow-funded (escrow-id uint))
    (match (map-get? escrows escrow-id)
        escrow (ok (is-eq (get status escrow) "FUNDED"))
        err-escrow-not-found
    )
)

(define-read-only (get-total-escrows)
    (var-get last-escrow-id)
)

(define-read-only (get-total-disputes)
    (var-get last-dispute-id)
)


