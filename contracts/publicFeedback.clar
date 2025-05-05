
;; title: publicFeedback


(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SCORE (err u101))
(define-constant ERR-SERVANT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RATED (err u103))

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