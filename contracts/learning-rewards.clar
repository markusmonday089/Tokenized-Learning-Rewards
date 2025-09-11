
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
(define-constant ERR-INVALID-REFERRER (err u106))
(define-constant ERR-ALREADY-REFERRED (err u107))
(define-constant ERR-SELF-REFERRAL (err u108))
(define-constant ERR-REFERRER-NOT-FOUND (err u109))

(define-data-var referral-reward-percentage uint u20)
(define-data-var minimum-milestone-points uint u50)
(define-data-var referral-code-counter uint u0)

(define-map user-referral-codes principal uint)
(define-map referral-code-owners uint principal)
(define-map user-referrers principal principal)
(define-map referrer-stats principal {
    total-referrals: uint,
    active-referrals: uint,
    total-rewards-earned: uint,
    successful-referrals: uint
})

(define-map referral-rewards { referrer: principal, referee: principal } uint)


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


(define-constant ERR-NO-ACTIVITY-TODAY (err u104))
(define-constant ERR-STREAK-ALREADY-CLAIMED (err u105))

(define-data-var streak-bonus-multiplier uint u10)
(define-data-var max-streak-bonus uint u1000)

(define-map user-streaks principal {
    current-streak: uint,
    longest-streak: uint,
    last-activity-day: uint,
    streak-tokens-earned: uint
})

(define-map daily-streak-claims { user: principal, day: uint } bool)

(define-read-only (get-current-day)
    (/ stacks-block-height u144)
)

(define-read-only (get-user-streak (user principal))
    (default-to 
        { current-streak: u0, longest-streak: u0, last-activity-day: u0, streak-tokens-earned: u0 }
        (map-get? user-streaks user)
    )
)

(define-read-only (min (a uint) (b uint))
    (if (<= a b) a b)
)

(define-read-only (calculate-streak-bonus (streak-days uint))
    (min (* streak-days (var-get streak-bonus-multiplier)) (var-get max-streak-bonus))
)

(define-read-only (has-claimed-streak-today (user principal))
    (default-to false (map-get? daily-streak-claims { user: user, day: (get-current-day) }))
)

(define-read-only (max (a uint) (b uint))
    (if (>= a b) a b)
)

(define-private (update-user-streak (user principal))
    (let (
        (current-day (get-current-day))
        (user-streak (get-user-streak user))
        (last-day (get last-activity-day user-streak))
        (current-streak-count (get current-streak user-streak))
        (new-streak (if (is-eq (+ last-day u1) current-day)
                       (+ current-streak-count u1)
                       u1))
    )
        (map-set user-streaks user {
            current-streak: new-streak,
            longest-streak: (max new-streak (get longest-streak user-streak)),
            last-activity-day: current-day,
            streak-tokens-earned: (get streak-tokens-earned user-streak)
        })
        new-streak
    )
)

(define-public (claim-daily-streak-bonus)
    (let (
        (current-day (get-current-day))
        (user-streak (get-user-streak tx-sender))
        (user-progresss (get-user-progress tx-sender))
    )
        (asserts! (not (has-claimed-streak-today tx-sender)) ERR-STREAK-ALREADY-CLAIMED)
        (asserts! (is-eq (get last-activity-day user-streak) current-day) ERR-NO-ACTIVITY-TODAY)
        (let (
            (streak-bonus (calculate-streak-bonus (get current-streak user-streak)))
        )
            (map-set daily-streak-claims { user: tx-sender, day: current-day } true)
            (map-set user-streaks tx-sender (merge user-streak {
                streak-tokens-earned: (+ (get streak-tokens-earned user-streak) streak-bonus)
            }))
            (ft-mint? learning-token streak-bonus tx-sender)
        )
    )
)

(define-public (set-streak-bonus-multiplier (new-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set streak-bonus-multiplier new-multiplier))
    )
)

(define-public (set-max-streak-bonus (new-max uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set max-streak-bonus new-max))
    )
)

(define-public (claim-milestone-with-streak (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones milestone-id) ERR-INVALID-MILESTONE))
        (current-progress (get-user-progress tx-sender))
        (claim-status (has-claimed-milestone tx-sender milestone-id))
        (new-streak (update-user-streak tx-sender))
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

(define-read-only (get-referral-code (user principal))
    (map-get? user-referral-codes user)
)

(define-read-only (get-referral-code-owner (referral-code uint))
    (map-get? referral-code-owners referral-code)
)

(define-read-only (get-user-referrer (user principal))
    (map-get? user-referrers user)
)

(define-read-only (get-referrer-stats (referrer principal))
    (default-to 
        { total-referrals: u0, active-referrals: u0, total-rewards-earned: u0, successful-referrals: u0 }
        (map-get? referrer-stats referrer)
    )
)

(define-read-only (get-referral-reward-earned (referrer principal) (referee principal))
    (default-to u0 (map-get? referral-rewards { referrer: referrer, referee: referee }))
)

(define-read-only (calculate-referral-reward (milestone-points uint))
    (/ (* milestone-points (var-get referral-reward-percentage)) u100)
)

(define-public (generate-referral-code)
    (let (
        (new-code (+ (var-get referral-code-counter) u1))
        (user tx-sender)
    )
        (asserts! (is-none (get-referral-code user)) ERR-ALREADY-REFERRED)
        (map-set user-referral-codes user new-code)
        (map-set referral-code-owners new-code user)
        (var-set referral-code-counter new-code)
        (ok new-code)
    )
)

(define-public (register-with-referrer (referral-code uint))
    (let (
        (referrer (unwrap! (get-referral-code-owner referral-code) ERR-REFERRER-NOT-FOUND))
        (referee tx-sender)
    )
        (asserts! (not (is-eq referrer referee)) ERR-SELF-REFERRAL)
        (asserts! (is-none (get-user-referrer referee)) ERR-ALREADY-REFERRED)
        (map-set user-referrers referee referrer)
        (let (
            (current-stats (get-referrer-stats referrer))
        )
            (map-set referrer-stats referrer {
                total-referrals: (+ (get total-referrals current-stats) u1),
                active-referrals: (+ (get active-referrals current-stats) u1),
                total-rewards-earned: (get total-rewards-earned current-stats),
                successful-referrals: (get successful-referrals current-stats)
            })
        )
        (ok true)
    )
)

(define-public (claim-milestone-with-referral (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones milestone-id) ERR-INVALID-MILESTONE))
        (current-progress (get-user-progress tx-sender))
        (claim-status (has-claimed-milestone tx-sender milestone-id))
        (milestone-points (get points milestone))
    )
        (asserts! (not claim-status) ERR-ALREADY-CLAIMED)
        (map-set milestone-claims { user: tx-sender, milestone: milestone-id } true)
        (map-set user-progress tx-sender {
            completed-milestones: (unwrap-panic (as-max-len? (append (get completed-milestones current-progress) milestone-id) u100)),
            total-points: (+ (get total-points current-progress) milestone-points),
            tokens-earned: (+ (get tokens-earned current-progress) milestone-points)
        })
        (unwrap-panic (ft-mint? learning-token milestone-points tx-sender))
        (match (get-user-referrer tx-sender)
            referrer (begin
                (if (>= milestone-points (var-get minimum-milestone-points))
                    (let (
                        (referral-reward (calculate-referral-reward milestone-points))
                        (current-reward (get-referral-reward-earned referrer tx-sender))
                        (current-stats (get-referrer-stats referrer))
                    )
                        (map-set referral-rewards { referrer: referrer, referee: tx-sender } (+ current-reward referral-reward))
                        (map-set referrer-stats referrer {
                            total-referrals: (get total-referrals current-stats),
                            active-referrals: (get active-referrals current-stats),
                            total-rewards-earned: (+ (get total-rewards-earned current-stats) referral-reward),
                            successful-referrals: (+ (get successful-referrals current-stats) u1)
                        })
                        (unwrap-panic (ft-mint? learning-token referral-reward referrer))
                    )
                    false
                )
            )
            false
        )
        (ok true)
    )
)

(define-public (set-referral-reward-percentage (new-percentage uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-percentage u50) ERR-NOT-AUTHORIZED)
        (ok (var-set referral-reward-percentage new-percentage))
    )
)

(define-public (set-minimum-milestone-points (new-minimum uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set minimum-milestone-points new-minimum))
    )
)

;; Learning Time Banking System
;; Users can log study time, accumulate time credits, and exchange for rewards

(define-constant ERR-INVALID-TIME-AMOUNT (err u110))
(define-constant ERR-INSUFFICIENT-TIME-CREDITS (err u111))
(define-constant ERR-INVALID-SESSION (err u112))
(define-constant ERR-SESSION-TOO-LONG (err u113))
(define-constant ERR-INVALID-EXCHANGE-RATE (err u114))

;; Time banking configuration variables
(define-data-var time-to-token-rate uint u5) ;; 5 minutes = 1 token
(define-data-var max-session-minutes uint u480) ;; 8 hours max per session
(define-data-var daily-time-bonus-threshold uint u120) ;; 2 hours daily bonus threshold
(define-data-var time-bonus-multiplier uint u15) ;; 15% bonus for hitting daily threshold

;; Time credit system - tracks accumulated study time in minutes
(define-map user-time-credits principal {
    total-minutes: uint,
    available-credits: uint,
    redeemed-credits: uint,
    sessions-completed: uint
})

;; Daily time tracking for bonus calculations
(define-map daily-time-logs { user: principal, day: uint } {
    minutes-logged: uint,
    sessions-count: uint,
    bonus-claimed: bool
})

;; Study session records for analytics
(define-map study-sessions uint {
    user: principal,
    start-block: uint,
    minutes-logged: uint,
    subject-category: (string-utf8 32),
    day: uint
})

;; Time transfer system for peer collaboration
(define-map time-transfers { sender: principal, recipient: principal, session-id: uint } uint)

;; Session counter for unique session IDs
(define-data-var session-counter uint u0)

;; Read-only functions for time banking system
(define-read-only (get-user-time-credits (user principal))
    (default-to 
        { total-minutes: u0, available-credits: u0, redeemed-credits: u0, sessions-completed: u0 }
        (map-get? user-time-credits user)
    )
)

(define-read-only (get-daily-time-log (user principal) (day uint))
    (default-to 
        { minutes-logged: u0, sessions-count: u0, bonus-claimed: false }
        (map-get? daily-time-logs { user: user, day: day })
    )
)

(define-read-only (get-study-session (session-id uint))
    (map-get? study-sessions session-id)
)

(define-read-only (get-time-transfer (sender principal) (recipient principal) (session-id uint))
    (default-to u0 (map-get? time-transfers { sender: sender, recipient: recipient, session-id: session-id }))
)

(define-read-only (calculate-time-token-value (minutes uint))
    (/ minutes (var-get time-to-token-rate))
)

(define-read-only (calculate-daily-bonus (minutes uint))
    (if (>= minutes (var-get daily-time-bonus-threshold))
        (/ (* minutes (var-get time-bonus-multiplier)) u100)
        u0
    )
)

(define-read-only (get-current-learning-day)
    (/ stacks-block-height u144) ;; Assumes 144 blocks per day
)

;; Core time banking functions
(define-public (log-study-session (minutes uint) (subject-category (string-utf8 32)))
    (let (
        (session-id (+ (var-get session-counter) u1))
        (current-day (get-current-learning-day))
        (user tx-sender)
        (current-credits (get-user-time-credits user))
        (daily-log (get-daily-time-log user current-day))
    )
        ;; Validate session parameters
        (asserts! (> minutes u0) ERR-INVALID-TIME-AMOUNT)
        (asserts! (<= minutes (var-get max-session-minutes)) ERR-SESSION-TOO-LONG)
        
        ;; Update session counter
        (var-set session-counter session-id)
        
        ;; Record the study session
        (map-set study-sessions session-id {
            user: user,
            start-block: stacks-block-height,
            minutes-logged: minutes,
            subject-category: subject-category,
            day: current-day
        })
        
        ;; Update user time credits
        (map-set user-time-credits user {
            total-minutes: (+ (get total-minutes current-credits) minutes),
            available-credits: (+ (get available-credits current-credits) minutes),
            redeemed-credits: (get redeemed-credits current-credits),
            sessions-completed: (+ (get sessions-completed current-credits) u1)
        })
        
        ;; Update daily time log
        (map-set daily-time-logs { user: user, day: current-day } {
            minutes-logged: (+ (get minutes-logged daily-log) minutes),
            sessions-count: (+ (get sessions-count daily-log) u1),
            bonus-claimed: (get bonus-claimed daily-log)
        })
        
        (ok session-id)
    )
)

(define-public (redeem-time-credits (credits-to-redeem uint))
    (let (
        (user tx-sender)
        (current-credits (get-user-time-credits user))
        (available (get available-credits current-credits))
        (tokens-to-mint (calculate-time-token-value credits-to-redeem))
    )
        ;; Validate redemption amount
        (asserts! (> credits-to-redeem u0) ERR-INVALID-TIME-AMOUNT)
        (asserts! (<= credits-to-redeem available) ERR-INSUFFICIENT-TIME-CREDITS)
        
        ;; Update user credits
        (map-set user-time-credits user {
            total-minutes: (get total-minutes current-credits),
            available-credits: (- available credits-to-redeem),
            redeemed-credits: (+ (get redeemed-credits current-credits) credits-to-redeem),
            sessions-completed: (get sessions-completed current-credits)
        })
        
        ;; Mint tokens based on time credits
        (ft-mint? learning-token tokens-to-mint user)
    )
)

(define-public (claim-daily-time-bonus)
    (let (
        (current-day (get-current-learning-day))
        (user tx-sender)
        (daily-log (get-daily-time-log user current-day))
        (minutes-today (get minutes-logged daily-log))
        (bonus-amount (calculate-daily-bonus minutes-today))
    )
        ;; Check if bonus can be claimed
        (asserts! (not (get bonus-claimed daily-log)) ERR-ALREADY-CLAIMED)
        (asserts! (>= minutes-today (var-get daily-time-bonus-threshold)) ERR-INVALID-TIME-AMOUNT)
        
        ;; Mark bonus as claimed
        (map-set daily-time-logs { user: user, day: current-day } {
            minutes-logged: minutes-today,
            sessions-count: (get sessions-count daily-log),
            bonus-claimed: true
        })
        
        ;; Mint bonus tokens
        (ft-mint? learning-token bonus-amount user)
    )
)

(define-public (transfer-time-credits (recipient principal) (credits-amount uint) (session-id uint))
    (let (
        (sender tx-sender)
        (sender-credits (get-user-time-credits sender))
        (recipient-credits (get-user-time-credits recipient))
        (available (get available-credits sender-credits))
    )
        ;; Validate transfer
        (asserts! (> credits-amount u0) ERR-INVALID-TIME-AMOUNT)
        (asserts! (<= credits-amount available) ERR-INSUFFICIENT-TIME-CREDITS)
        (asserts! (not (is-eq sender recipient)) ERR-SELF-REFERRAL)
        
        ;; Update sender credits
        (map-set user-time-credits sender {
            total-minutes: (get total-minutes sender-credits),
            available-credits: (- available credits-amount),
            redeemed-credits: (get redeemed-credits sender-credits),
            sessions-completed: (get sessions-completed sender-credits)
        })
        
        ;; Update recipient credits
        (map-set user-time-credits recipient {
            total-minutes: (+ (get total-minutes recipient-credits) credits-amount),
            available-credits: (+ (get available-credits recipient-credits) credits-amount),
            redeemed-credits: (get redeemed-credits recipient-credits),
            sessions-completed: (get sessions-completed recipient-credits)
        })
        
        ;; Record the transfer
        (map-set time-transfers { sender: sender, recipient: recipient, session-id: session-id } credits-amount)
        
        (ok true)
    )
)

(define-public (bulk-redeem-weekly-time)
    (let (
        (user tx-sender)
        (current-credits (get-user-time-credits user))
        (available (get available-credits current-credits))
        (weekly-threshold u2100) ;; 35 hours = bonus rate
        (bonus-rate u110) ;; 10% bonus for bulk weekly redemption
    )
        ;; Validate bulk redemption
        (asserts! (>= available weekly-threshold) ERR-INSUFFICIENT-TIME-CREDITS)
        
        (let (
            (base-tokens (calculate-time-token-value available))
            (bonus-tokens (/ (* base-tokens bonus-rate) u100))
            (total-tokens (+ base-tokens bonus-tokens))
        )
            ;; Update user credits
            (map-set user-time-credits user {
                total-minutes: (get total-minutes current-credits),
                available-credits: u0,
                redeemed-credits: (+ (get redeemed-credits current-credits) available),
                sessions-completed: (get sessions-completed current-credits)
            })
            
            ;; Mint tokens with bonus
            (ft-mint? learning-token total-tokens user)
        )
    )
)

;; Owner configuration functions
(define-public (set-time-to-token-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-rate u0) ERR-INVALID-EXCHANGE-RATE)
        (ok (var-set time-to-token-rate new-rate))
    )
)

(define-public (set-max-session-minutes (new-max uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-max u0) ERR-INVALID-TIME-AMOUNT)
        (ok (var-set max-session-minutes new-max))
    )
)

(define-public (set-daily-time-bonus-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set daily-time-bonus-threshold new-threshold))
    )
)

(define-public (set-time-bonus-multiplier (new-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-multiplier u50) ERR-NOT-AUTHORIZED)
        (ok (var-set time-bonus-multiplier new-multiplier))
    )
)



