
;; title: learning-rewards
;; version: 1.0


(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-CLAIMED (err u101))
(define-constant ERR-INVALID-MILESTONE (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))

(define-fungible-token learning-token)

(define-data-var token-uri (string-utf8 256) u"https://learning-rewards.xyz/token")
(define-data-var contract-owner principal tx-sender)
(define-data-var total-milestones uint u100)

(define-map milestones uint {
    name: (string-utf8 64),
    points: uint,
    difficulty: uint
})

(define-map user-progress principal {
    completed-milestones: (list 100 uint),
    total-points: uint,
    tokens-earned: uint
})

(define-map milestone-claims { user: principal, milestone: uint } bool)

(define-read-only (get-token-uri)
    (var-get token-uri)
)

(define-read-only (get-milestone (milestone-id uint))
    (map-get? milestones milestone-id)
)

(define-read-only (get-user-progress (user principal))
    (default-to 
        { completed-milestones: (list), total-points: u0, tokens-earned: u0 }
        (map-get? user-progress user)
    )
)

(define-read-only (has-claimed-milestone (user principal) (milestone-id uint))
    (default-to false (map-get? milestone-claims { user: user, milestone: milestone-id }))
)

(define-public (set-milestone (milestone-id uint) (milestone-data {name: (string-utf8 64), points: uint, difficulty: uint}))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= milestone-id (var-get total-milestones)) ERR-INVALID-MILESTONE)
        (ok (map-set milestones milestone-id milestone-data))
    )
)

(define-public (claim-milestone (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones milestone-id) ERR-INVALID-MILESTONE))
        (current-progress (get-user-progress tx-sender))
        (claim-status (has-claimed-milestone tx-sender milestone-id))
    )
        (asserts! (not claim-status) ERR-ALREADY-CLAIMED)
        (map-set milestone-claims { user: tx-sender, milestone: milestone-id } true)
        (map-set user-progress tx-sender {
            completed-milestones: (unwrap-panic (as-max-len? (append (get completed-milestones current-progress) milestone-id) u100)),
            total-points: (+ (get total-points current-progress) (get points milestone)),
            tokens-earned: (+ (get tokens-earned current-progress) (get points milestone))
        })
        (ft-mint? learning-token (get points milestone) tx-sender)
    )
)

(define-public (transfer-tokens (amount uint) (recipient principal))
    (begin
        (asserts! (>= (ft-get-balance learning-token tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
        (ft-transfer? learning-token amount tx-sender recipient)
    )
)

(define-public (set-token-uri (new-uri (string-utf8 256)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set token-uri new-uri))
    )
)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)


(define-map milestone-categories uint (string-utf8 32))

(define-map milestone-category-mapping uint uint)

(define-read-only (get-milestone-category (category-id uint))
    (map-get? milestone-categories category-id)
)

(define-read-only (get-milestones-by-category (category-id uint))
    (map-get? milestone-category-mapping category-id)
)

(define-public (set-milestone-category (category-id uint) (category-name (string-utf8 32)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set milestone-categories category-id category-name))
    )
)

(define-public (assign-milestone-category (milestone-id uint) (category-id uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= milestone-id (var-get total-milestones)) ERR-INVALID-MILESTONE)
        (ok (map-set milestone-category-mapping milestone-id category-id))
    )
)


(define-map badges uint {
    name: (string-utf8 64),
    description: (string-utf8 256),
    required-points: uint
})

(define-map user-badges { user: principal, badge-id: uint } bool)

(define-read-only (get-badge (badge-id uint))
    (map-get? badges badge-id)
)

(define-read-only (has-badge (user principal) (badge-id uint))
    (default-to false (map-get? user-badges { user: user, badge-id: badge-id }))
)

(define-public (create-badge (badge-id uint) (badge-data { name: (string-utf8 64), description: (string-utf8 256), required-points: uint }))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set badges badge-id badge-data))
    )
)

(define-public (check-and-award-badge (badge-id uint))
    (let (
        (badge (unwrap! (map-get? badges badge-id) ERR-INVALID-MILESTONE))
        (user-stats (get-user-progress tx-sender))
    )
        (asserts! (>= (get total-points user-stats) (get required-points badge)) ERR-NOT-AUTHORIZED)
        (ok (map-set user-badges { user: tx-sender, badge-id: badge-id } true))
    )
)