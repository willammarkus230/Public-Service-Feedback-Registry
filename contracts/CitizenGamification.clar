;; Citizen Engagement Gamification System
;; Incentivizes meaningful feedback through points, badges, and leaderboards

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-input (err u402))
(define-constant err-badge-exists (err u403))
(define-constant err-insufficient-points (err u404))

;; Badge types
(define-constant badge-first-feedback u1)
(define-constant badge-consistent-reviewer u2)
(define-constant badge-quality-contributor u3)
(define-constant badge-community-leader u4)
(define-constant badge-feedback-champion u5)

;; Point values
(define-constant points-per-feedback u10)
(define-constant points-quality-bonus u5)
(define-constant points-consistency-bonus u3)
(define-constant points-monthly-bonus u25)

;; Data variables
(define-data-var gamification-enabled bool true)
(define-data-var season-start-block uint u0)
(define-data-var current-season uint u1)

;; Citizen gamification profiles
(define-map citizen-profiles
  { citizen: principal }
  {
    total-points: uint,
    current-season-points: uint,
    feedback-count: uint,
    quality-score: uint,
    consistency-streak: uint,
    last-feedback-block: uint,
    reputation-level: uint,
    active-badges: (list 10 uint)
  }
)

;; Badge definitions
(define-map badge-registry
  { badge-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    requirement: uint,
    reward-points: uint,
    rarity-tier: uint,
    active: bool
  }
)

;; Badge ownership
(define-map citizen-badges
  { citizen: principal, badge-id: uint }
  {
    earned-at: uint,
    season-earned: uint,
    progress: uint
  }
)

;; Leaderboard tracking
(define-map season-leaderboard
  { season: uint, position: uint }
  {
    citizen: principal,
    points: uint,
    feedback-count: uint
  }
)

;; Monthly challenges
(define-map monthly-challenges
  { challenge-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    target-metric: uint,
    reward-points: uint,
    start-block: uint,
    end-block: uint,
    participants: uint,
    completed: uint,
    active: bool
  }
)

;; Challenge participation
(define-map challenge-participation
  { citizen: principal, challenge-id: uint }
  {
    joined-at: uint,
    progress: uint,
    completed: bool,
    reward-claimed: bool
  }
)

;; Achievement milestones
(define-map achievement-milestones
  { milestone-id: uint }
  {
    description: (string-ascii 150),
    points-threshold: uint,
    feedback-threshold: uint,
    reward-points: uint,
    special-badge: (optional uint)
  }
)

;; Read-only functions
(define-read-only (get-citizen-profile (citizen principal))
  (match (map-get? citizen-profiles { citizen: citizen })
    profile (ok profile)
    (err err-not-found)
  )
)

(define-read-only (get-badge-info (badge-id uint))
  (match (map-get? badge-registry { badge-id: badge-id })
    badge (ok badge)
    (err err-not-found)
  )
)

(define-read-only (has-badge (citizen principal) (badge-id uint))
  (is-some (map-get? citizen-badges { citizen: citizen, badge-id: badge-id }))
)

(define-read-only (get-leaderboard-position (season uint) (position uint))
  (match (map-get? season-leaderboard { season: season, position: position })
    entry (ok entry)
    (err err-not-found)
  )
)

(define-read-only (get-challenge-progress (citizen principal) (challenge-id uint))
  (match (map-get? challenge-participation { citizen: citizen, challenge-id: challenge-id })
    progress (ok progress)
    (err err-not-found)
  )
)

;; Public functions
(define-public (award-feedback-points (citizen principal) (feedback-quality uint))
  (let 
    ((profile (default-to default-profile (map-get? citizen-profiles { citizen: citizen })))
     (quality-bonus (if (>= feedback-quality u4) points-quality-bonus u0))
     (consistency-bonus (if (> (get consistency-streak profile) u5) points-consistency-bonus u0))
     (total-points-earned (+ points-per-feedback quality-bonus consistency-bonus))
     (new-streak (if (< (- stacks-block-height (get last-feedback-block profile)) u1000) 
                    (+ (get consistency-streak profile) u1) 
                    u1)))
    
    (asserts! (var-get gamification-enabled) (ok u0))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (map-set citizen-profiles { citizen: citizen }
      (merge profile {
        total-points: (+ (get total-points profile) total-points-earned),
        current-season-points: (+ (get current-season-points profile) total-points-earned),
        feedback-count: (+ (get feedback-count profile) u1),
        quality-score: (calculate-quality-score (get quality-score profile) feedback-quality (get feedback-count profile)),
        consistency-streak: new-streak,
        last-feedback-block: stacks-block-height,
        reputation-level: (calculate-reputation-level (+ (get total-points profile) total-points-earned))
      }))
    
    ;; Check for badge eligibility
    (try! (check-and-award-badges citizen))
    
    ;; Update challenge progress
    (unwrap! (update-challenge-progress citizen) (ok total-points-earned))
    
    (ok total-points-earned)
  )
)

(define-public (initialize-badge-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    ;; First Feedback Badge
    (map-set badge-registry { badge-id: badge-first-feedback }
      { name: "First Steps", description: "Submitted your first feedback", requirement: u1, reward-points: u5, rarity-tier: u1, active: true })
    
    ;; Consistent Reviewer Badge
    (map-set badge-registry { badge-id: badge-consistent-reviewer }
      { name: "Consistent Voice", description: "Maintained feedback streak for 10 submissions", requirement: u10, reward-points: u20, rarity-tier: u2, active: true })
    
    ;; Quality Contributor Badge
    (map-set badge-registry { badge-id: badge-quality-contributor }
      { name: "Quality First", description: "Achieved high quality scores on 25 feedbacks", requirement: u25, reward-points: u50, rarity-tier: u3, active: true })
    
    ;; Community Leader Badge
    (map-set badge-registry { badge-id: badge-community-leader }
      { name: "Community Leader", description: "Reached 500 total points", requirement: u500, reward-points: u100, rarity-tier: u4, active: true })
    
    ;; Feedback Champion Badge
    (map-set badge-registry { badge-id: badge-feedback-champion }
      { name: "Feedback Champion", description: "Submitted 100 quality feedbacks", requirement: u100, reward-points: u200, rarity-tier: u5, active: true })
    
    (var-set season-start-block stacks-block-height)
    (ok true)
  )
)

(define-public (create-monthly-challenge (title (string-ascii 100)) (description (string-ascii 300)) (target-metric uint) (reward-points uint) (duration-blocks uint))
  (let 
    ((challenge-id (+ (get-next-challenge-id) u1))
     (start-block stacks-block-height)
     (end-block (+ start-block duration-blocks)))
    
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (> target-metric u0) err-invalid-input)
    (asserts! (> reward-points u0) err-invalid-input)
    
    (map-set monthly-challenges { challenge-id: challenge-id }
      {
        title: title,
        description: description,
        target-metric: target-metric,
        reward-points: reward-points,
        start-block: start-block,
        end-block: end-block,
        participants: u0,
        completed: u0,
        active: true
      })
    
    (ok challenge-id)
  )
)

(define-public (join-challenge (challenge-id uint))
  (let 
    ((challenge (unwrap! (map-get? monthly-challenges { challenge-id: challenge-id }) err-not-found))
     (existing-participation (map-get? challenge-participation { citizen: tx-sender, challenge-id: challenge-id })))
    
    (asserts! (is-none existing-participation) err-badge-exists)
    (asserts! (get active challenge) err-invalid-input)
    (asserts! (< stacks-block-height (get end-block challenge)) err-invalid-input)
    
    (map-set challenge-participation { citizen: tx-sender, challenge-id: challenge-id }
      {
        joined-at: stacks-block-height,
        progress: u0,
        completed: false,
        reward-claimed: false
      })
    
    (map-set monthly-challenges { challenge-id: challenge-id }
      (merge challenge { participants: (+ (get participants challenge) u1) }))
    
    (ok true)
  )
)

(define-public (update-leaderboard (season uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    ;; Simplified leaderboard update - in practice would sort by points
    (ok true)
  )
)

(define-public (toggle-gamification (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set gamification-enabled enabled)
    (ok enabled)
  )
)

;; Private helper functions
(define-private (check-and-award-badges (citizen principal))
  (let 
    ((profile (unwrap! (map-get? citizen-profiles { citizen: citizen }) err-not-found))
     (feedback-count (get feedback-count profile))
     (quality-score (get quality-score profile))
     (total-points (get total-points profile))
     (consistency-streak (get consistency-streak profile)))
    
    ;; Check first feedback badge
    (if (and (>= feedback-count u1) (not (has-badge citizen badge-first-feedback)))
      (begin (unwrap! (award-badge citizen badge-first-feedback) (ok false)) true)
      false)
    
    ;; Check consistent reviewer badge
    (if (and (>= consistency-streak u10) (not (has-badge citizen badge-consistent-reviewer)))
      (begin (unwrap! (award-badge citizen badge-consistent-reviewer) (ok false)) true)
      false)
    
    ;; Check quality contributor badge
    (if (and (>= feedback-count u25) (>= quality-score u400) (not (has-badge citizen badge-quality-contributor)))
      (begin (unwrap! (award-badge citizen badge-quality-contributor) (ok false)) true)
      false)
    
    ;; Check community leader badge
    (if (and (>= total-points u500) (not (has-badge citizen badge-community-leader)))
      (begin (unwrap! (award-badge citizen badge-community-leader) (ok false)) true)
      false)
    
    ;; Check feedback champion badge
    (if (and (>= feedback-count u100) (>= quality-score u450) (not (has-badge citizen badge-feedback-champion)))
      (begin (unwrap! (award-badge citizen badge-feedback-champion) (ok false)) true)
      false)
    
    (ok true)
  )
)

(define-private (award-badge (citizen principal) (badge-id uint))
  (let 
    ((badge (unwrap! (map-get? badge-registry { badge-id: badge-id }) err-not-found))
     (profile (unwrap! (map-get? citizen-profiles { citizen: citizen }) err-not-found)))
    
    (map-set citizen-badges { citizen: citizen, badge-id: badge-id }
      {
        earned-at: stacks-block-height,
        season-earned: (var-get current-season),
        progress: u100
      })
    
    ;; Add badge points
    (map-set citizen-profiles { citizen: citizen }
      (merge profile {
        total-points: (+ (get total-points profile) (get reward-points badge)),
        active-badges: (unwrap! (as-max-len? (append (get active-badges profile) badge-id) u10) err-invalid-input)
      }))
    
    (ok true)
  )
)

(define-private (update-challenge-progress (citizen principal))
  ;; Simplified challenge progress update
  (ok u1)
)

(define-private (calculate-quality-score (current-score uint) (new-rating uint) (feedback-count uint))
  (if (is-eq feedback-count u0)
    (* new-rating u100)
    (+ (/ (* current-score u9) u10) (/ (* new-rating u100) u10)))
)

(define-private (calculate-reputation-level (total-points uint))
  (if (< total-points u100) u1
    (if (< total-points u300) u2
      (if (< total-points u600) u3
        (if (< total-points u1000) u4
          u5))))
)

(define-private (get-next-challenge-id)
  ;; Simplified ID generation
  u1
)

;; Default profile for new citizens
(define-constant default-profile {
  total-points: u0,
  current-season-points: u0,
  feedback-count: u0,
  quality-score: u0,
  consistency-streak: u0,
  last-feedback-block: u0,
  reputation-level: u1,
  active-badges: (list)
})
