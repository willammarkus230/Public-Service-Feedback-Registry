
;; title: publicFeedback


(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SCORE (err u101))
(define-constant ERR-SERVANT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RATED (err u103))


(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u105))
(define-constant ERR-THRESHOLD-NOT-MET (err u106))
(define-constant ERR-INVALID-REWARD-TYPE (err u107))

(define-constant REWARD-EXCELLENCE u1)
(define-constant REWARD-CONSISTENCY u2)
(define-constant REWARD-IMPROVEMENT u3)
(define-constant REWARD-MILESTONE u4)

(define-data-var reward-fund uint u1000000)
(define-data-var next-reward-id uint u1)

(define-map servant-rewards
  { servant-id: uint }
  {
    excellence-rewards: uint,
    consistency-rewards: uint,
    improvement-rewards: uint,
    milestone-rewards: uint,
    total-rewards: uint,
    last-reward-height: uint,
    lifetime-earnings: uint
  }
)

(define-map reward-history
  { reward-id: uint }
  {
    servant-id: uint,
    reward-type: uint,
    amount: uint,
    reason: (string-utf8 200),
    timestamp: uint,
    claimed: bool
  }
)

(define-map reward-thresholds
  { reward-type: uint }
  {
    min-reviews: uint,
    min-score: uint,
    reward-amount: uint,
    active: bool
  }
)

(define-map pending-rewards
  { servant-id: uint, reward-type: uint }
  {
    amount: uint,
    earned-height: uint,
    expires-height: uint,
    qualified: bool
  }
)

(define-map servant-performance-history
  { servant-id: uint }
  {
    last-month-score: uint,
    last-month-reviews: uint,
    current-month-score: uint,
    current-month-reviews: uint,
    performance-trend: uint,
    last-calculated: uint
  }
)


(define-data-var admin principal tx-sender)
(define-data-var next-servant-id uint u1)

(define-map civil-servants
  { servant-id: uint }
  {
    name: (string-ascii 50),
    department: (string-ascii 50),
    position: (string-ascii 50),
    active: bool
  }
)

(define-map feedback-scores
  { servant-id: uint, reviewer: principal }
  {
    score: uint,
    comment: (string-utf8 500),
    timestamp: uint
  }
)

(define-map servant-stats
  { servant-id: uint }
  {
    total-scores: uint,
    total-reviews: uint,
    average-score: uint
  }
)

(define-read-only (get-civil-servant (servant-id uint))
  (map-get? civil-servants { servant-id: servant-id })
)

(define-read-only (get-feedback (servant-id uint) (reviewer principal))
  (map-get? feedback-scores { servant-id: servant-id, reviewer: reviewer })
)

(define-read-only (get-servant-stats (servant-id uint))
  (map-get? servant-stats { servant-id: servant-id })
)

(define-public (add-civil-servant (name (string-ascii 50)) (department (string-ascii 50)) (position (string-ascii 50)))
  (let
    (
      (new-id (var-get next-servant-id))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set civil-servants
      { servant-id: new-id }
      {
        name: name,
        department: department,
        position: position,
        active: true
      }
    )
    (map-set servant-stats
      { servant-id: new-id }
      {
        total-scores: u0,
        total-reviews: u0,
        average-score: u0
      }
    )
    (var-set next-servant-id (+ new-id u1))
    (ok new-id)
  )
)

(define-public (deactivate-servant (servant-id uint))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set civil-servants
      { servant-id: servant-id }
      (merge servant { active: false })
    )
    (ok true)
  )
)

(define-public (submit-feedback (servant-id uint) (score uint) (comment (string-utf8 500)))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (stats (unwrap! (get-servant-stats servant-id) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-none (get-feedback servant-id tx-sender)) ERR-ALREADY-RATED)
    (asserts! (and (>= score u1) (<= score u5)) ERR-INVALID-SCORE)
    (asserts! (get active servant) ERR-SERVANT-NOT-FOUND)
    
    (map-set feedback-scores
      { servant-id: servant-id, reviewer: tx-sender }
      {
        score: score,
        comment: comment,
        timestamp: stacks-block-height
      }
    )
    
    (map-set servant-stats
      { servant-id: servant-id }
      {
        total-scores: (+ (get total-scores stats) score),
        total-reviews: (+ (get total-reviews stats) u1),
        average-score: (/ (+ (get total-scores stats) score) (+ (get total-reviews stats) u1))
      }
    )
    (ok true)
  )
)

(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (var-set admin new-admin)
    (ok true)
  )
)



(define-map servant-responses
  { servant-id: uint, reviewer: principal }
  {
    response: (string-utf8 500),
    timestamp: uint
  }
)

(define-read-only (get-servant-response (servant-id uint) (reviewer principal))
  (map-get? servant-responses { servant-id: servant-id, reviewer: reviewer })
)

(define-public (respond-to-feedback (servant-id uint) (reviewer principal) (response (string-utf8 500)))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (feedback (unwrap! (get-feedback servant-id reviewer) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set servant-responses
      { servant-id: servant-id, reviewer: reviewer }
      {
        response: response,
        timestamp: stacks-block-height
      }
    )
    (ok true)
  )
)


(define-map servant-analytics
  { servant-id: uint }
  {
    one-star-count: uint,
    two-star-count: uint,
    three-star-count: uint,
    four-star-count: uint,
    five-star-count: uint,
    last-review-height: uint
  }
)

(define-read-only (get-servant-analytics (servant-id uint))
  (map-get? servant-analytics { servant-id: servant-id })
)

(define-public (initialize-analytics (servant-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set servant-analytics
      { servant-id: servant-id }
      {
        one-star-count: u0,
        two-star-count: u0,
        three-star-count: u0,
        four-star-count: u0,
        five-star-count: u0,
        last-review-height: u0
      }
    )
    (ok true)
  )
)

(define-private (update-analytics (servant-id uint) (score uint))
  (let
    (
      (analytics (unwrap! (get-servant-analytics servant-id) ERR-SERVANT-NOT-FOUND))
    )
    (map-set servant-analytics
      { servant-id: servant-id }
      (merge analytics
        {
          one-star-count: (if (is-eq score u1) (+ (get one-star-count analytics) u1) (get one-star-count analytics)),
          two-star-count: (if (is-eq score u2) (+ (get two-star-count analytics) u1) (get two-star-count analytics)),
          three-star-count: (if (is-eq score u3) (+ (get three-star-count analytics) u1) (get three-star-count analytics)),
          four-star-count: (if (is-eq score u4) (+ (get four-star-count analytics) u1) (get four-star-count analytics)),
          five-star-count: (if (is-eq score u5) (+ (get five-star-count analytics) u1) (get five-star-count analytics)),
          last-review-height: stacks-block-height
        }
      )
    )
    (ok true)
  )
)


(define-map department-stats
  { department: (string-ascii 50) }
  {
    total-servants: uint,
    active-servants: uint,
    total-reviews: uint,
    total-scores: uint,
    average-score: uint,
    last-updated: uint
  }
)

(define-map department-score-distribution
  { department: (string-ascii 50) }
  {
    one-star-total: uint,
    two-star-total: uint,
    three-star-total: uint,
    four-star-total: uint,
    five-star-total: uint
  }
)

(define-read-only (get-department-stats (department (string-ascii 50)))
  (map-get? department-stats { department: department })
)

(define-read-only (get-department-score-distribution (department (string-ascii 50)))
  (map-get? department-score-distribution { department: department })
)

(define-public (initialize-department (department (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set department-stats
      { department: department }
      {
        total-servants: u0,
        active-servants: u0,
        total-reviews: u0,
        total-scores: u0,
        average-score: u0,
        last-updated: stacks-block-height
      }
    )
    (map-set department-score-distribution
      { department: department }
      {
        one-star-total: u0,
        two-star-total: u0,
        three-star-total: u0,
        four-star-total: u0,
        five-star-total: u0
      }
    )
    (ok true)
  )
)

(define-private (update-department-stats (department (string-ascii 50)) (score uint))
  (let
    (
      (dept-stats (unwrap! (get-department-stats department) ERR-SERVANT-NOT-FOUND))
      (dept-distribution (unwrap! (get-department-score-distribution department) ERR-SERVANT-NOT-FOUND))
      (new-total-reviews (+ (get total-reviews dept-stats) u1))
      (new-total-scores (+ (get total-scores dept-stats) score))
    )
    (map-set department-stats
      { department: department }
      (merge dept-stats
        {
          total-reviews: new-total-reviews,
          total-scores: new-total-scores,
          average-score: (/ new-total-scores new-total-reviews),
          last-updated: stacks-block-height
        }
      )
    )
    (map-set department-score-distribution
      { department: department }
      (merge dept-distribution
        {
          one-star-total: (if (is-eq score u1) (+ (get one-star-total dept-distribution) u1) (get one-star-total dept-distribution)),
          two-star-total: (if (is-eq score u2) (+ (get two-star-total dept-distribution) u1) (get two-star-total dept-distribution)),
          three-star-total: (if (is-eq score u3) (+ (get three-star-total dept-distribution) u1) (get three-star-total dept-distribution)),
          four-star-total: (if (is-eq score u4) (+ (get four-star-total dept-distribution) u1) (get four-star-total dept-distribution)),
          five-star-total: (if (is-eq score u5) (+ (get five-star-total dept-distribution) u1) (get five-star-total dept-distribution))
        }
      )
    )
    (ok true)
  )
)

(define-private (increment-department-servant-count (department (string-ascii 50)))
  (let
    (
      (dept-stats (default-to 
        {
          total-servants: u0,
          active-servants: u0,
          total-reviews: u0,
          total-scores: u0,
          average-score: u0,
          last-updated: stacks-block-height
        }
        (get-department-stats department)
      ))
    )
    (map-set department-stats
      { department: department }
      (merge dept-stats
        {
          total-servants: (+ (get total-servants dept-stats) u1),
          active-servants: (+ (get active-servants dept-stats) u1),
          last-updated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-private (decrement-department-active-count (department (string-ascii 50)))
  (let
    (
      (dept-stats (unwrap! (get-department-stats department) ERR-SERVANT-NOT-FOUND))
    )
    (map-set department-stats
      { department: department }
      (merge dept-stats
        {
          active-servants: (if (> (get active-servants dept-stats) u0) 
                             (- (get active-servants dept-stats) u1) 
                             u0),
          last-updated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-public (submit-feedback-new (servant-id uint) (score uint) (comment (string-utf8 500)))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (stats (unwrap! (get-servant-stats servant-id) ERR-SERVANT-NOT-FOUND))
      (department (get department servant))
    )
    (asserts! (is-none (get-feedback servant-id tx-sender)) ERR-ALREADY-RATED)
    (asserts! (and (>= score u1) (<= score u5)) ERR-INVALID-SCORE)
    (asserts! (get active servant) ERR-SERVANT-NOT-FOUND)
    
    (map-set feedback-scores
      { servant-id: servant-id, reviewer: tx-sender }
      {
        score: score,
        comment: comment,
        timestamp: stacks-block-height
      }
    )
    
    (map-set servant-stats
      { servant-id: servant-id }
      {
        total-scores: (+ (get total-scores stats) score),
        total-reviews: (+ (get total-reviews stats) u1),
        average-score: (/ (+ (get total-scores stats) score) (+ (get total-reviews stats) u1))
      }
    )
    
    (try! (update-department-stats department score))
    (ok true)
  )
)

(define-public (deactivate-servant-new (servant-id uint))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (department (get department servant))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set civil-servants
      { servant-id: servant-id }
      (merge servant { active: false })
    )
    (try! (decrement-department-active-count department))
    (ok true)
  )
)

(define-read-only (get-servant-rewards (servant-id uint))
  (map-get? servant-rewards { servant-id: servant-id })
)

(define-read-only (get-reward-history (reward-id uint))
  (map-get? reward-history { reward-id: reward-id })
)

(define-read-only (get-reward-thresholds (reward-type uint))
  (map-get? reward-thresholds { reward-type: reward-type })
)

(define-read-only (get-pending-rewards (servant-id uint) (reward-type uint))
  (map-get? pending-rewards { servant-id: servant-id, reward-type: reward-type })
)

(define-read-only (get-servant-performance-history (servant-id uint))
  (map-get? servant-performance-history { servant-id: servant-id })
)

(define-read-only (get-reward-fund-balance)
  (ok (var-get reward-fund))
)

(define-public (initialize-reward-system)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set reward-thresholds
      { reward-type: REWARD-EXCELLENCE }
      {
        min-reviews: u10,
        min-score: u450,
        reward-amount: u1000,
        active: true
      }
    )
    (map-set reward-thresholds
      { reward-type: REWARD-CONSISTENCY }
      {
        min-reviews: u20,
        min-score: u400,
        reward-amount: u750,
        active: true
      }
    )
    (map-set reward-thresholds
      { reward-type: REWARD-IMPROVEMENT }
      {
        min-reviews: u15,
        min-score: u350,
        reward-amount: u500,
        active: true
      }
    )
    (map-set reward-thresholds
      { reward-type: REWARD-MILESTONE }
      {
        min-reviews: u50,
        min-score: u375,
        reward-amount: u2000,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (initialize-servant-rewards (servant-id uint))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set servant-rewards
      { servant-id: servant-id }
      {
        excellence-rewards: u0,
        consistency-rewards: u0,
        improvement-rewards: u0,
        milestone-rewards: u0,
        total-rewards: u0,
        last-reward-height: u0,
        lifetime-earnings: u0
      }
    )
    (map-set servant-performance-history
      { servant-id: servant-id }
      {
        last-month-score: u0,
        last-month-reviews: u0,
        current-month-score: u0,
        current-month-reviews: u0,
        performance-trend: u0,
        last-calculated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-private (calculate-reward-eligibility (servant-id uint))
  (let
    (
      (stats (unwrap! (get-servant-stats servant-id) ERR-SERVANT-NOT-FOUND))
      (total-reviews (get total-reviews stats))
      (total-scores (get total-scores stats))
      (average-score (get average-score stats))
      (excellence-threshold (unwrap! (get-reward-thresholds REWARD-EXCELLENCE) ERR-INVALID-REWARD-TYPE))
      (consistency-threshold (unwrap! (get-reward-thresholds REWARD-CONSISTENCY) ERR-INVALID-REWARD-TYPE))
      (improvement-threshold (unwrap! (get-reward-thresholds REWARD-IMPROVEMENT) ERR-INVALID-REWARD-TYPE))
      (milestone-threshold (unwrap! (get-reward-thresholds REWARD-MILESTONE) ERR-INVALID-REWARD-TYPE))
    )
    (if (and (>= total-reviews (get min-reviews milestone-threshold)) 
             (>= total-scores (get min-score milestone-threshold)))
        (begin
          (unwrap! (queue-reward servant-id REWARD-MILESTONE (get reward-amount milestone-threshold) u"Milestone achievement") ERR-INVALID-REWARD-TYPE)
          (ok true)
        )
        (if (and (>= total-reviews (get min-reviews excellence-threshold)) 
                 (>= total-scores (get min-score excellence-threshold)))
            (begin
              (unwrap! (queue-reward servant-id REWARD-EXCELLENCE (get reward-amount excellence-threshold) u"Excellence in service") ERR-INVALID-REWARD-TYPE)
              (ok true)
            )
            (if (and (>= total-reviews (get min-reviews consistency-threshold)) 
                     (>= total-scores (get min-score consistency-threshold)))
                (begin
                  (unwrap! (queue-reward servant-id REWARD-CONSISTENCY (get reward-amount consistency-threshold) u"Consistent performance") ERR-INVALID-REWARD-TYPE)
                  (ok true)
                )
                (if (and (>= total-reviews (get min-reviews improvement-threshold)) 
                         (>= total-scores (get min-score improvement-threshold)))
                    (begin
                      (unwrap! (queue-reward servant-id REWARD-IMPROVEMENT (get reward-amount improvement-threshold) u"Performance improvement") ERR-INVALID-REWARD-TYPE)
                      (ok true)
                    )
                    (ok false)
                )
            )
        )
    )
  )
)

(define-private (queue-reward (servant-id uint) (reward-type uint) (amount uint) (reason (string-utf8 200)))
  (let
    (
      (current-height stacks-block-height)
      (expiry-height (+ current-height u1000))
    )
    (map-set pending-rewards
      { servant-id: servant-id, reward-type: reward-type }
      {
        amount: amount,
        earned-height: current-height,
        expires-height: expiry-height,
        qualified: true
      }
    )
    (ok true)
  )
)

(define-public (claim-reward (servant-id uint) (reward-type uint))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (pending (unwrap! (get-pending-rewards servant-id reward-type) ERR-REWARD-ALREADY-CLAIMED))
      (current-rewards (unwrap! (get-servant-rewards servant-id) ERR-SERVANT-NOT-FOUND))
      (reward-amount (get amount pending))
      (current-height stacks-block-height)
      (new-reward-id (var-get next-reward-id))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (get qualified pending) ERR-THRESHOLD-NOT-MET)
    (asserts! (< current-height (get expires-height pending)) ERR-REWARD-ALREADY-CLAIMED)
    (asserts! (>= (var-get reward-fund) reward-amount) ERR-INSUFFICIENT-BALANCE)
    
    (map-set reward-history
      { reward-id: new-reward-id }
      {
        servant-id: servant-id,
        reward-type: reward-type,
        amount: reward-amount,
        reason: (if (is-eq reward-type REWARD-EXCELLENCE) u"Excellence in service"
                  (if (is-eq reward-type REWARD-CONSISTENCY) u"Consistent performance"
                    (if (is-eq reward-type REWARD-IMPROVEMENT) u"Performance improvement"
                      u"Milestone achievement"))),
        timestamp: current-height,
        claimed: true
      }
    )
    
    (map-set servant-rewards
      { servant-id: servant-id }
      (merge current-rewards
        {
          excellence-rewards: (if (is-eq reward-type REWARD-EXCELLENCE) 
                                (+ (get excellence-rewards current-rewards) u1) 
                                (get excellence-rewards current-rewards)),
          consistency-rewards: (if (is-eq reward-type REWARD-CONSISTENCY) 
                                 (+ (get consistency-rewards current-rewards) u1) 
                                 (get consistency-rewards current-rewards)),
          improvement-rewards: (if (is-eq reward-type REWARD-IMPROVEMENT) 
                                 (+ (get improvement-rewards current-rewards) u1) 
                                 (get improvement-rewards current-rewards)),
          milestone-rewards: (if (is-eq reward-type REWARD-MILESTONE) 
                               (+ (get milestone-rewards current-rewards) u1) 
                               (get milestone-rewards current-rewards)),
          total-rewards: (+ (get total-rewards current-rewards) u1),
          last-reward-height: current-height,
          lifetime-earnings: (+ (get lifetime-earnings current-rewards) reward-amount)
        }
      )
    )
    
    (map-delete pending-rewards { servant-id: servant-id, reward-type: reward-type })
    (var-set reward-fund (- (var-get reward-fund) reward-amount))
    (var-set next-reward-id (+ new-reward-id u1))
    (ok reward-amount)
  )
)

(define-public (update-reward-threshold (reward-type uint) (min-reviews uint) (min-score uint) (reward-amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq reward-type REWARD-EXCELLENCE) 
                  (is-eq reward-type REWARD-CONSISTENCY) 
                  (is-eq reward-type REWARD-IMPROVEMENT) 
                  (is-eq reward-type REWARD-MILESTONE)) ERR-INVALID-REWARD-TYPE)
    (map-set reward-thresholds
      { reward-type: reward-type }
      {
        min-reviews: min-reviews,
        min-score: min-score,
        reward-amount: reward-amount,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (fund-reward-system (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (var-set reward-fund (+ (var-get reward-fund) amount))
    (ok (var-get reward-fund))
  )
)

(define-public (evaluate-servant-for-rewards (servant-id uint))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (get active servant) ERR-SERVANT-NOT-FOUND)
    (try! (calculate-reward-eligibility servant-id))
    (ok true)
  )
)

(define-public (submit-feedback-with-rewards (servant-id uint) (score uint) (comment (string-utf8 500)))
  (let
    (
      (servant (unwrap! (get-civil-servant servant-id) ERR-SERVANT-NOT-FOUND))
      (stats (unwrap! (get-servant-stats servant-id) ERR-SERVANT-NOT-FOUND))
      (department (get department servant))
    )
    (asserts! (is-none (get-feedback servant-id tx-sender)) ERR-ALREADY-RATED)
    (asserts! (and (>= score u1) (<= score u5)) ERR-INVALID-SCORE)
    (asserts! (get active servant) ERR-SERVANT-NOT-FOUND)
    
    (map-set feedback-scores
      { servant-id: servant-id, reviewer: tx-sender }
      {
        score: score,
        comment: comment,
        timestamp: stacks-block-height
      }
    )
    
    (map-set servant-stats
      { servant-id: servant-id }
      {
        total-scores: (+ (get total-scores stats) score),
        total-reviews: (+ (get total-reviews stats) u1),
        average-score: (/ (+ (get total-scores stats) score) (+ (get total-reviews stats) u1))
      }
    )
    
    (try! (update-department-stats department score))
    (try! (calculate-reward-eligibility servant-id))
    (ok true)
  )
)

(define-constant ERR-ESCALATION-NOT-FOUND (err u108))
(define-constant ERR-ESCALATION-ALREADY-EXISTS (err u109))
(define-constant ERR-ESCALATION-CLOSED (err u110))
(define-constant ERR-INVALID-ESCALATION-STATUS (err u111))
(define-constant ERR-ESCALATION-DEADLINE-PASSED (err u112))
(define-constant ERR-INSUFFICIENT-RATING (err u113))
(define-constant ERR-ESCALATION-LIMIT-REACHED (err u114))

(define-constant ESCALATION-PENDING u1)
(define-constant ESCALATION-INVESTIGATING u2)
(define-constant ESCALATION-RESOLVED u3)
(define-constant ESCALATION-DISMISSED u4)

(define-constant ESCALATION-URGENT u1)
(define-constant ESCALATION-HIGH u2)
(define-constant ESCALATION-MEDIUM u3)
(define-constant ESCALATION-LOW u4)

(define-data-var next-escalation-id uint u1)
(define-data-var escalation-deadline-blocks uint u1000)
(define-data-var max-escalations-per-citizen uint u3)

(define-map citizen-escalations
  { escalation-id: uint }
  {
    citizen: principal,
    servant-id: uint,
    original-feedback-score: uint,
    escalation-reason: (string-utf8 1000),
    evidence: (string-utf8 2000),
    priority-level: uint,
    status: uint,
    created-height: uint,
    deadline-height: uint,
    assigned-investigator: (optional principal),
    resolution-notes: (optional (string-utf8 1000)),
    resolved-height: (optional uint),
    follow-up-required: bool,
    citizen-satisfaction: (optional uint),
    escalation-category: (string-ascii 50)
  }
)

(define-map escalation-updates
  { escalation-id: uint, update-id: uint }
  {
    updater: principal,
    status-change: uint,
    notes: (string-utf8 500),
    timestamp: uint,
    action-taken: (string-utf8 300),
    next-steps: (string-utf8 300)
  }
)

(define-map citizen-escalation-history
  { citizen: principal }
  {
    total-escalations: uint,
    pending-escalations: uint,
    resolved-escalations: uint,
    dismissed-escalations: uint,
    last-escalation-height: uint,
    escalation-success-rate: uint,
    reputation-score: uint,
    restriction-until-height: (optional uint)
  }
)

(define-map escalation-investigators
  { investigator: principal }
  {
    assigned-cases: uint,
    resolved-cases: uint,
    pending-cases: uint,
    specialization: (string-ascii 100),
    active: bool,
    workload-capacity: uint,
    performance-rating: uint,
    last-assignment-height: uint
  }
)

(define-map escalation-categories
  { category: (string-ascii 50) }
  {
    total-escalations: uint,
    resolved-escalations: uint,
    average-resolution-time: uint,
    priority-weight: uint,
    active: bool,
    department-focus: (string-ascii 50)
  }
)

(define-map escalation-metrics
  { metric-type: (string-ascii 30) }
  {
    total-count: uint,
    monthly-count: uint,
    average-resolution-time: uint,
    success-rate: uint,
    last-updated: uint
  }
)

(define-read-only (get-escalation (escalation-id uint))
  (map-get? citizen-escalations { escalation-id: escalation-id })
)

(define-read-only (get-escalation-update (escalation-id uint) (update-id uint))
  (map-get? escalation-updates { escalation-id: escalation-id, update-id: update-id })
)

(define-read-only (get-citizen-escalation-history (citizen principal))
  (map-get? citizen-escalation-history { citizen: citizen })
)

(define-read-only (get-escalation-investigator (investigator principal))
  (map-get? escalation-investigators { investigator: investigator })
)

(define-read-only (get-escalation-category (category (string-ascii 50)))
  (map-get? escalation-categories { category: category })
)

(define-read-only (get-escalation-metrics (metric-type (string-ascii 30)))
  (map-get? escalation-metrics { metric-type: metric-type })
)

(define-read-only (check-escalation-eligibility (citizen principal) (servant-id uint))
  (let
    (
      (citizen-history (get-citizen-escalation-history citizen))
      (servant (get-civil-servant servant-id))
      (feedback (get-feedback servant-id citizen))
    )
    (if (is-some servant)
      (let
        (
          (servant-data (unwrap-panic servant))
        )
        (if (is-some citizen-history)
          (let
            (
              (history (unwrap-panic citizen-history))
              (pending-count (get pending-escalations history))
              (restriction-height (get restriction-until-height history))
            )
            (and
              (< pending-count (var-get max-escalations-per-citizen))
              (if (is-some restriction-height)
                (< stacks-block-height (unwrap-panic restriction-height))
                true)
              (is-some feedback)
              (get active servant-data)
            )
          )
          (and
            (is-some feedback)
            (get active servant-data)
          )
        )
      )
      false
    )
  )
)

(define-public (initialize-escalation-system)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set escalation-metrics
      { metric-type: "total-system" }
      {
        total-count: u0,
        monthly-count: u0,
        average-resolution-time: u0,
        success-rate: u0,
        last-updated: stacks-block-height
      }
    )
    (map-set escalation-categories
      { category: "service-quality" }
      {
        total-escalations: u0,
        resolved-escalations: u0,
        average-resolution-time: u0,
        priority-weight: u3,
        active: true,
        department-focus: "general"
      }
    )
    (map-set escalation-categories
      { category: "misconduct" }
      {
        total-escalations: u0,
        resolved-escalations: u0,
        average-resolution-time: u0,
        priority-weight: u1,
        active: true,
        department-focus: "general"
      }
    )
    (map-set escalation-categories
      { category: "discrimination" }
      {
        total-escalations: u0,
        resolved-escalations: u0,
        average-resolution-time: u0,
        priority-weight: u1,
        active: true,
        department-focus: "general"
      }
    )
    (ok true)
  )
)

(define-public (register-escalation-investigator (investigator principal) (specialization (string-ascii 100)) (capacity uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set escalation-investigators
      { investigator: investigator }
      {
        assigned-cases: u0,
        resolved-cases: u0,
        pending-cases: u0,
        specialization: specialization,
        active: true,
        workload-capacity: capacity,
        performance-rating: u350,
        last-assignment-height: u0
      }
    )
    (ok true)
  )
)

(define-public (create-escalation (servant-id uint) (escalation-reason (string-utf8 1000)) (evidence (string-utf8 2000)) (priority-level uint) (category (string-ascii 50)))
  (let
    (
      (escalation-id (var-get next-escalation-id))
      (citizen-feedback (unwrap! (get-feedback servant-id tx-sender) ERR-SERVANT-NOT-FOUND))
      (original-score (get score citizen-feedback))
      (current-height stacks-block-height)
      (deadline-height (+ current-height (var-get escalation-deadline-blocks)))
      (citizen-history (get-citizen-escalation-history tx-sender))
    )
    (asserts! (check-escalation-eligibility tx-sender servant-id) ERR-ESCALATION-LIMIT-REACHED)
    (asserts! (<= original-score u2) ERR-INSUFFICIENT-RATING)
    (asserts! (and (>= priority-level ESCALATION-LOW) (<= priority-level ESCALATION-URGENT)) ERR-INVALID-ESCALATION-STATUS)
    (asserts! (is-some (get-escalation-category category)) ERR-ESCALATION-NOT-FOUND)
    
    (map-set citizen-escalations
      { escalation-id: escalation-id }
      {
        citizen: tx-sender,
        servant-id: servant-id,
        original-feedback-score: original-score,
        escalation-reason: escalation-reason,
        evidence: evidence,
        priority-level: priority-level,
        status: ESCALATION-PENDING,
        created-height: current-height,
        deadline-height: deadline-height,
        assigned-investigator: none,
        resolution-notes: none,
        resolved-height: none,
        follow-up-required: false,
        citizen-satisfaction: none,
        escalation-category: category
      }
    )
    
    (if (is-some citizen-history)
      (let
        (
          (history (unwrap-panic citizen-history))
        )
        (map-set citizen-escalation-history
          { citizen: tx-sender }
          (merge history
            {
              total-escalations: (+ (get total-escalations history) u1),
              pending-escalations: (+ (get pending-escalations history) u1),
              last-escalation-height: current-height
            }
          )
        )
      )
      (map-set citizen-escalation-history
        { citizen: tx-sender }
        {
          total-escalations: u1,
          pending-escalations: u1,
          resolved-escalations: u0,
          dismissed-escalations: u0,
          last-escalation-height: current-height,
          escalation-success-rate: u0,
          reputation-score: u300,
          restriction-until-height: none
        }
      )
    )
    
    (try! (update-escalation-category-stats category))
    (try! (update-escalation-metrics))
    (var-set next-escalation-id (+ escalation-id u1))
    (ok escalation-id)
  )
)

(define-public (assign-escalation-investigator (escalation-id uint) (investigator principal))
  (let
    (
      (escalation (unwrap! (get-escalation escalation-id) ERR-ESCALATION-NOT-FOUND))
      (investigator-profile (unwrap! (get-escalation-investigator investigator) ERR-SERVANT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escalation) ESCALATION-PENDING) ERR-ESCALATION-CLOSED)
    (asserts! (get active investigator-profile) ERR-SERVANT-NOT-FOUND)
    (asserts! (< (get pending-cases investigator-profile) (get workload-capacity investigator-profile)) ERR-ESCALATION-LIMIT-REACHED)
    
    (map-set citizen-escalations
      { escalation-id: escalation-id }
      (merge escalation
        {
          status: ESCALATION-INVESTIGATING,
          assigned-investigator: (some investigator)
        }
      )
    )
    
    (map-set escalation-investigators
      { investigator: investigator }
      (merge investigator-profile
        {
          assigned-cases: (+ (get assigned-cases investigator-profile) u1),
          pending-cases: (+ (get pending-cases investigator-profile) u1),
          last-assignment-height: stacks-block-height
        }
      )
    )
    
    (unwrap! (add-escalation-update escalation-id ESCALATION-INVESTIGATING u"Case assigned to investigator" u"Investigation commenced" u"Gather evidence and interview parties") ERR-ESCALATION-NOT-FOUND)
    (ok true)
  )
)

(define-public (resolve-escalation (escalation-id uint) (resolution-notes (string-utf8 1000)) (follow-up-required bool))
  (let
    (
      (escalation (unwrap! (get-escalation escalation-id) ERR-ESCALATION-NOT-FOUND))
      (investigator (unwrap! (get assigned-investigator escalation) ERR-NOT-AUTHORIZED))
      (investigator-profile (unwrap! (get-escalation-investigator investigator) ERR-SERVANT-NOT-FOUND))
      (citizen (get citizen escalation))
      (citizen-history (unwrap! (get-citizen-escalation-history citizen) ERR-SERVANT-NOT-FOUND))
      (current-height stacks-block-height)
      (resolution-time (- current-height (get created-height escalation)))
    )
    (asserts! (or (is-eq tx-sender (var-get admin)) (is-eq tx-sender investigator)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escalation) ESCALATION-INVESTIGATING) ERR-ESCALATION-CLOSED)
    
    (map-set citizen-escalations
      { escalation-id: escalation-id }
      (merge escalation
        {
          status: ESCALATION-RESOLVED,
          resolution-notes: (some resolution-notes),
          resolved-height: (some current-height),
          follow-up-required: follow-up-required
        }
      )
    )
    
    (map-set escalation-investigators
      { investigator: investigator }
      (merge investigator-profile
        {
          resolved-cases: (+ (get resolved-cases investigator-profile) u1),
          pending-cases: (- (get pending-cases investigator-profile) u1),
          performance-rating: (+ (get performance-rating investigator-profile) u10)
        }
      )
    )
    
    (map-set citizen-escalation-history
      { citizen: citizen }
      (merge citizen-history
        {
          pending-escalations: (- (get pending-escalations citizen-history) u1),
          resolved-escalations: (+ (get resolved-escalations citizen-history) u1),
          escalation-success-rate: (/ (* (+ (get resolved-escalations citizen-history) u1) u100) (get total-escalations citizen-history))
        }
      )
    )
    
    (unwrap! (add-escalation-update escalation-id ESCALATION-RESOLVED u"Case resolved by investigator" u"Case resolved by investigator" u"Monitoring for follow-up if required") ERR-ESCALATION-NOT-FOUND)
    (unwrap! (update-escalation-metrics) ERR-ESCALATION-NOT-FOUND)
    (ok resolution-time)
  )
)

(define-public (dismiss-escalation (escalation-id uint) (dismissal-reason (string-utf8 1000)))
  (let
    (
      (escalation (unwrap! (get-escalation escalation-id) ERR-ESCALATION-NOT-FOUND))
      (citizen (get citizen escalation))
      (citizen-history (unwrap! (get-citizen-escalation-history citizen) ERR-SERVANT-NOT-FOUND))
      (investigator-opt (get assigned-investigator escalation))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq (get status escalation) ESCALATION-PENDING) (is-eq (get status escalation) ESCALATION-INVESTIGATING)) ERR-ESCALATION-CLOSED)
    
    (map-set citizen-escalations
      { escalation-id: escalation-id }
      (merge escalation
        {
          status: ESCALATION-DISMISSED,
          resolution-notes: (some dismissal-reason),
          resolved-height: (some stacks-block-height)
        }
      )
    )
    
    (map-set citizen-escalation-history
      { citizen: citizen }
      (merge citizen-history
        {
          pending-escalations: (- (get pending-escalations citizen-history) u1),
          dismissed-escalations: (+ (get dismissed-escalations citizen-history) u1),
          reputation-score: (if (> (get reputation-score citizen-history) u50) (- (get reputation-score citizen-history) u50) u0)
        }
      )
    )
    
    (if (is-some investigator-opt)
      (let
        (
          (investigator (unwrap-panic investigator-opt))
          (investigator-profile (unwrap! (get-escalation-investigator investigator) ERR-SERVANT-NOT-FOUND))
        )
        (map-set escalation-investigators
          { investigator: investigator }
          (merge investigator-profile
            {
              pending-cases: (- (get pending-cases investigator-profile) u1)
            }
          )
        )
      )
      true
    )
    
    (unwrap! (add-escalation-update escalation-id ESCALATION-DISMISSED u"Escalation dismissed by administrator" u"Escalation dismissed by administrator" u"No further action required") ERR-ESCALATION-NOT-FOUND)
    (ok true)
  )
)

(define-public (rate-escalation-resolution (escalation-id uint) (satisfaction-rating uint))
  (let
    (
      (escalation (unwrap! (get-escalation escalation-id) ERR-ESCALATION-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get citizen escalation)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escalation) ESCALATION-RESOLVED) ERR-ESCALATION-CLOSED)
    (asserts! (and (>= satisfaction-rating u1) (<= satisfaction-rating u5)) ERR-INVALID-SCORE)
    
    (map-set citizen-escalations
      { escalation-id: escalation-id }
      (merge escalation
        {
          citizen-satisfaction: (some satisfaction-rating)
        }
      )
    )
    (ok true)
  )
)

(define-private (add-escalation-update (escalation-id uint) (status-change uint) (notes (string-utf8 500)) (action-taken (string-utf8 300)) (next-steps (string-utf8 300)))
  (let
    (
      (update-id (+ escalation-id u1))
    )
    (map-set escalation-updates
      { escalation-id: escalation-id, update-id: update-id }
      {
        updater: tx-sender,
        status-change: status-change,
        notes: notes,
        timestamp: stacks-block-height,
        action-taken: action-taken,
        next-steps: next-steps
      }
    )
    (ok true)
  )
)

(define-private (update-escalation-category-stats (category (string-ascii 50)))
  (let
    (
      (category-stats (unwrap! (get-escalation-category category) ERR-ESCALATION-NOT-FOUND))
    )
    (map-set escalation-categories
      { category: category }
      (merge category-stats
        {
          total-escalations: (+ (get total-escalations category-stats) u1)
        }
      )
    )
    (ok true)
  )
)

(define-private (update-escalation-metrics)
  (let
    (
      (metrics (unwrap! (get-escalation-metrics "total-system") ERR-ESCALATION-NOT-FOUND))
    )
    (map-set escalation-metrics
      { metric-type: "total-system" }
      (merge metrics
        {
          total-count: (+ (get total-count metrics) u1),
          monthly-count: (+ (get monthly-count metrics) u1),
          last-updated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-public (configure-escalation-settings (deadline-blocks uint) (max-escalations uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (var-set escalation-deadline-blocks deadline-blocks)
    (var-set max-escalations-per-citizen max-escalations)
    (ok true)
  )
)