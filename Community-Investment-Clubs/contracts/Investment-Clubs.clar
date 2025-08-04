;; Community Investment Clubs Smart Contract
;; A decentralized platform for managing community investment clubs

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-club-full (err u105))
(define-constant err-already-member (err u106))
(define-constant err-not-member (err u107))
(define-constant err-invalid-proposal (err u108))
(define-constant err-already-voted (err u109))
(define-constant err-voting-closed (err u110))

;; Data Variables
(define-data-var next-club-id uint u1)
(define-data-var next-proposal-id uint u1)

;; Data Maps
(define-map clubs
    { id: uint }
    {
        name: (string-ascii 64),
        creator: principal,
        max-members: uint,
        min-investment: uint,
        total-funds: uint,
        member-count: uint,
        active: bool
    }
)

(define-map club-members
    { club-id: uint, member: principal }
    {
        investment: uint,
        join-block: uint,
        active: bool
    }
)

(define-map investment-proposals
    { id: uint }
    {
        club-id: uint,
        proposer: principal,
        title: (string-ascii 64),
        description: (string-ascii 256),
        amount: uint,
        target: (string-ascii 64),
        votes-for: uint,
        votes-against: uint,
        voting-deadline: uint,
        executed: bool,
        active: bool
    }
)

(define-map proposal-votes
    { proposal-id: uint, voter: principal }
    { vote: bool, block-height: uint }
)

(define-map member-investments
    { club-id: uint, member: principal, investment-id: uint }
    {
        amount: uint,
        timestamp: uint,
        returns: uint
    }
)

;; Private Functions
(define-private (is-club-member (club-id uint) (member principal))
    (match (map-get? club-members { club-id: club-id, member: member })
        member-data (get active member-data)
        false
    )
)

(define-private (get-club-member-count (club-id uint))
    (match (map-get? clubs { id: club-id })
        club-data (get member-count club-data)
        u0
    )
)

(define-private (calculate-voting-power (club-id uint) (member principal))
    (match (map-get? club-members { club-id: club-id, member: member })
        member-data (get investment member-data)
        u0
    )
)

;; Public Functions

;; Create a new investment club
(define-public (create-club (name (string-ascii 64)) (max-members uint) (min-investment uint))
    (let
        (
            (club-id (var-get next-club-id))
        )
        (asserts! (> max-members u0) err-invalid-amount)
        (asserts! (> min-investment u0) err-invalid-amount)
        
        (map-set clubs
            { id: club-id }
            {
                name: name,
                creator: tx-sender,
                max-members: max-members,
                min-investment: min-investment,
                total-funds: u0,
                member-count: u0,
                active: true
            }
        )
        
        (var-set next-club-id (+ club-id u1))
        (ok club-id)
    )
)

;; Join an investment club
(define-public (join-club (club-id uint) (investment-amount uint))
    (let
        (
            (club-data (unwrap! (map-get? clubs { id: club-id }) err-not-found))
            (current-member-count (get member-count club-data))
        )
        (asserts! (get active club-data) err-not-found)
        (asserts! (< current-member-count (get max-members club-data)) err-club-full)
        (asserts! (>= investment-amount (get min-investment club-data)) err-invalid-amount)
        (asserts! (not (is-club-member club-id tx-sender)) err-already-member)
        
        ;; Transfer funds to contract (simplified - in real implementation would use STX transfers)
        (asserts! (>= investment-amount u1) err-insufficient-funds)
        
        ;; Add member
        (map-set club-members
            { club-id: club-id, member: tx-sender }
            {
                investment: investment-amount,
                join-block: block-height,
                active: true
            }
        )
        
        ;; Update club data
        (map-set clubs
            { id: club-id }
            (merge club-data {
                total-funds: (+ (get total-funds club-data) investment-amount),
                member-count: (+ current-member-count u1)
            })
        )
        
        (ok true)
    )
)

;; Create investment proposal
(define-public (create-proposal 
    (club-id uint) 
    (title (string-ascii 64)) 
    (description (string-ascii 256))
    (amount uint)
    (target (string-ascii 64))
    (voting-duration uint))
    (let
        (
            (proposal-id (var-get next-proposal-id))
            (club-data (unwrap! (map-get? clubs { id: club-id }) err-not-found))
        )
        (asserts! (get active club-data) err-not-found)
        (asserts! (is-club-member club-id tx-sender) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount (get total-funds club-data)) err-insufficient-funds)
        (asserts! (> voting-duration u0) err-invalid-proposal)
        
        (map-set investment-proposals
            { id: proposal-id }
            {
                club-id: club-id,
                proposer: tx-sender,
                title: title,
                description: description,
                amount: amount,
                target: target,
                votes-for: u0,
                votes-against: u0,
                voting-deadline: (+ block-height voting-duration),
                executed: false,
                active: true
            }
        )
        
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
    (let
        (
            (proposal-data (unwrap! (map-get? investment-proposals { id: proposal-id }) err-not-found))
            (club-id (get club-id proposal-data))
            (voting-power (calculate-voting-power club-id tx-sender))
        )
        (asserts! (get active proposal-data) err-not-found)
        (asserts! (is-club-member club-id tx-sender) err-not-member)
        (asserts! (<= block-height (get voting-deadline proposal-data)) err-voting-closed)
        (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) err-already-voted)
        (asserts! (> voting-power u0) err-unauthorized)
        
        ;; Record vote
        (map-set proposal-votes
            { proposal-id: proposal-id, voter: tx-sender }
            { vote: vote, block-height: block-height }
        )
        
        ;; Update proposal vote counts
        (map-set investment-proposals
            { id: proposal-id }
            (merge proposal-data {
                votes-for: (if vote (+ (get votes-for proposal-data) voting-power) (get votes-for proposal-data)),
                votes-against: (if vote (get votes-against proposal-data) (+ (get votes-against proposal-data) voting-power))
            })
        )
        
        (ok true)
    )
)

;; Execute approved proposal
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal-data (unwrap! (map-get? investment-proposals { id: proposal-id }) err-not-found))
            (club-data (unwrap! (map-get? clubs { id: (get club-id proposal-data) }) err-not-found))
        )
        (asserts! (get active proposal-data) err-not-found)
        (asserts! (not (get executed proposal-data)) err-invalid-proposal)
        (asserts! (> block-height (get voting-deadline proposal-data)) err-voting-closed)
        (asserts! (> (get votes-for proposal-data) (get votes-against proposal-data)) err-unauthorized)
        (asserts! (is-club-member (get club-id proposal-data) tx-sender) err-not-member)
        
        ;; Mark as executed
        (map-set investment-proposals
            { id: proposal-id }
            (merge proposal-data { executed: true })
        )
        
        ;; Update club funds (simplified)
        (map-set clubs
            { id: (get club-id proposal-data) }
            (merge club-data {
                total-funds: (- (get total-funds club-data) (get amount proposal-data))
            })
        )
        
        (ok true)
    )
)

;; Leave club
(define-public (leave-club (club-id uint))
    (let
        (
            (member-data (unwrap! (map-get? club-members { club-id: club-id, member: tx-sender }) err-not-member))
            (club-data (unwrap! (map-get? clubs { id: club-id }) err-not-found))
        )
        (asserts! (get active member-data) err-not-member)
        
        ;; Deactivate membership
        (map-set club-members
            { club-id: club-id, member: tx-sender }
            (merge member-data { active: false })
        )
        
        ;; Update club member count
        (map-set clubs
            { id: club-id }
            (merge club-data {
                member-count: (- (get member-count club-data) u1),
                total-funds: (- (get total-funds club-data) (get investment member-data))
            })
        )
        
        (ok (get investment member-data))
    )
)

;; Read-only functions

(define-read-only (get-club (club-id uint))
    (map-get? clubs { id: club-id })
)

(define-read-only (get-club-member (club-id uint) (member principal))
    (map-get? club-members { club-id: club-id, member: member })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? investment-proposals { id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-next-club-id)
    (var-get next-club-id)
)

(define-read-only (get-next-proposal-id)
    (var-get next-proposal-id)
)