;; Learning Achievement NFT System for Tokenized-Learning-Rewards
;; Enables users to mint unique NFTs for major learning achievements

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u600))
(define-constant ERR_ACHIEVEMENT_NOT_FOUND (err u601))
(define-constant ERR_ALREADY_MINTED (err u602))
(define-constant ERR_REQUIREMENTS_NOT_MET (err u603))
(define-constant ERR_INVALID_ACHIEVEMENT (err u604))
(define-constant ERR_TOKEN_NOT_FOUND (err u605))
(define-constant ERR_NOT_TOKEN_OWNER (err u606))

;; Contract variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-token-id uint u1)
(define-data-var next-achievement-id uint u1)

;; NFT definition
(define-non-fungible-token learning-achievement-nft uint)

;; Achievement definitions with unlock criteria
(define-map achievement-types
  uint
  {
    name: (string-utf8 64),
    description: (string-utf8 200),
    image-uri: (string-utf8 256),
    rarity: (string-utf8 16),
    required-points: uint,
    required-streak: uint,
    required-time-hours: uint,
    required-milestones: uint,
    unlock-type: (string-utf8 20),
    is-active: bool
  }
)

;; NFT metadata for minted tokens
(define-map token-metadata
  uint
  {
    achievement-id: uint,
    owner: principal,
    minted-at: uint,
    achievement-date: uint,
    milestone-snapshot: uint,
    streak-snapshot: uint,
    time-snapshot: uint
  }
)

;; User achievement progress tracking
(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    unlocked: bool,
    unlocked-at: uint,
    nft-minted: bool,
    token-id: (optional uint)
  }
)

;; Achievement gallery for users
(define-map user-nft-gallery
  principal
  (list 50 uint)
)

;; Achievement rarity counts for tracking
(define-map rarity-stats
  (string-utf8 16)
  {
    total-minted: uint,
    unique-holders: uint
  }
)

;; Create new achievement type (admin only)
(define-public (create-achievement-type
  (name (string-utf8 64))
  (description (string-utf8 200))
  (image-uri (string-utf8 256))
  (rarity (string-utf8 16))
  (required-points uint)
  (required-streak uint)
  (required-time-hours uint)
  (required-milestones uint)
  (unlock-type (string-utf8 20)))
  (let
    (
      (achievement-id (var-get next-achievement-id))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_ACHIEVEMENT)
    
    (map-set achievement-types achievement-id
      {
        name: name,
        description: description,
        image-uri: image-uri,
        rarity: rarity,
        required-points: required-points,
        required-streak: required-streak,
        required-time-hours: required-time-hours,
        required-milestones: required-milestones,
        unlock-type: unlock-type,
        is-active: true
      }
    )
    
    (var-set next-achievement-id (+ achievement-id u1))
    (ok achievement-id)
  )
)

;; Check and unlock achievements for user
(define-public (check-achievement-unlock (achievement-id uint))
  (let
    (
      (achievement (unwrap! (map-get? achievement-types achievement-id) ERR_ACHIEVEMENT_NOT_FOUND))
      (user-progress (contract-call? .learning-rewards get-user-progress tx-sender))
      (user-streak (contract-call? .learning-rewards get-user-streak tx-sender))
      (user-time (contract-call? .learning-rewards get-user-time-credits tx-sender))
      (user-achievement (map-get? user-achievements { user: tx-sender, achievement-id: achievement-id }))
    )
    (asserts! (get is-active achievement) ERR_INVALID_ACHIEVEMENT)
    (asserts! (is-none user-achievement) ERR_ALREADY_MINTED)
    
    ;; Check if requirements are met
    (asserts! (>= (get total-points user-progress) (get required-points achievement)) ERR_REQUIREMENTS_NOT_MET)
    (asserts! (>= (get current-streak user-streak) (get required-streak achievement)) ERR_REQUIREMENTS_NOT_MET)
    (asserts! (>= (/ (get total-minutes user-time) u60) (get required-time-hours achievement)) ERR_REQUIREMENTS_NOT_MET)
    (asserts! (>= (len (get completed-milestones user-progress)) (get required-milestones achievement)) ERR_REQUIREMENTS_NOT_MET)
    
    ;; Unlock the achievement
    (map-set user-achievements { user: tx-sender, achievement-id: achievement-id }
      {
        unlocked: true,
        unlocked-at: stacks-block-height,
        nft-minted: false,
        token-id: none
      }
    )
    
    (ok true)
  )
)

;; Mint achievement NFT after unlocking
(define-public (mint-achievement-nft (achievement-id uint))
  (let
    (
      (achievement (unwrap! (map-get? achievement-types achievement-id) ERR_ACHIEVEMENT_NOT_FOUND))
      (user-achievement (unwrap! (map-get? user-achievements { user: tx-sender, achievement-id: achievement-id }) ERR_REQUIREMENTS_NOT_MET))
      (token-id (var-get next-token-id))
      (user-progress (contract-call? .learning-rewards get-user-progress tx-sender))
      (user-streak (contract-call? .learning-rewards get-user-streak tx-sender))
      (user-time (contract-call? .learning-rewards get-user-time-credits tx-sender))
      (user-gallery (default-to (list) (map-get? user-nft-gallery tx-sender)))
      (rarity (get rarity achievement))
      (current-rarity-stats (default-to { total-minted: u0, unique-holders: u0 } (map-get? rarity-stats rarity)))
    )
    (asserts! (get unlocked user-achievement) ERR_REQUIREMENTS_NOT_MET)
    (asserts! (not (get nft-minted user-achievement)) ERR_ALREADY_MINTED)
    
    ;; Mint the NFT
    (try! (nft-mint? learning-achievement-nft token-id tx-sender))
    
    ;; Store token metadata
    (map-set token-metadata token-id
      {
        achievement-id: achievement-id,
        owner: tx-sender,
        minted-at: stacks-block-height,
        achievement-date: (get unlocked-at user-achievement),
        milestone-snapshot: (get total-points user-progress),
        streak-snapshot: (get current-streak user-streak),
        time-snapshot: (get total-minutes user-time)
      }
    )
    
    ;; Update user achievement record
    (map-set user-achievements { user: tx-sender, achievement-id: achievement-id }
      (merge user-achievement { nft-minted: true, token-id: (some token-id) }))
    
    ;; Add to user gallery
    (map-set user-nft-gallery tx-sender
      (unwrap-panic (as-max-len? (append user-gallery token-id) u50)))
    
    ;; Update rarity statistics
    (map-set rarity-stats rarity
      {
        total-minted: (+ (get total-minted current-rarity-stats) u1),
        unique-holders: (+ (get unique-holders current-rarity-stats) u1)
      }
    )
    
    (var-set next-token-id (+ token-id u1))
    (ok token-id)
  )
)

;; Transfer NFT to another user
(define-public (transfer-achievement-nft (token-id uint) (recipient principal))
  (let
    (
      (token-metadata-data (unwrap! (map-get? token-metadata token-id) ERR_TOKEN_NOT_FOUND))
      (current-owner (unwrap! (nft-get-owner? learning-achievement-nft token-id) ERR_TOKEN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender current-owner) ERR_NOT_TOKEN_OWNER)
    
    ;; Transfer the NFT
    (try! (nft-transfer? learning-achievement-nft token-id tx-sender recipient))
    
    ;; Update token metadata
    (map-set token-metadata token-id
      (merge token-metadata-data { owner: recipient }))
    
    (ok true)
  )
)

;; Get NFT URI for metadata
(define-read-only (get-token-uri (token-id uint))
  (let
    (
      (metadata (unwrap! (map-get? token-metadata token-id) ERR_TOKEN_NOT_FOUND))
      (achievement (unwrap! (map-get? achievement-types (get achievement-id metadata)) ERR_ACHIEVEMENT_NOT_FOUND))
    )
    (ok (some (get image-uri achievement)))
  )
)

;; Read-only functions
(define-read-only (get-achievement-type (achievement-id uint))
  (map-get? achievement-types achievement-id)
)

(define-read-only (get-token-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

(define-read-only (get-user-achievement-status (user principal) (achievement-id uint))
  (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

(define-read-only (get-user-nft-gallery (user principal))
  (default-to (list) (map-get? user-nft-gallery user))
)

(define-read-only (get-rarity-stats (rarity (string-utf8 16)))
  (map-get? rarity-stats rarity)
)

(define-read-only (get-total-achievements)
  (- (var-get next-achievement-id) u1)
)

(define-read-only (get-total-nfts-minted)
  (- (var-get next-token-id) u1)
)
