(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_BOOK_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u102))
(define-constant ERR_INVALID_PRICE (err u103))
(define-constant ERR_BOOK_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_ROYALTY (err u105))
(define-constant ERR_PURCHASE_ALREADY_EXISTS (err u106))
(define-constant ERR_BOOK_NOT_PUBLISHED (err u107))
(define-constant ERR_REVIEW_ALREADY_EXISTS (err u108))
(define-constant ERR_INVALID_RATING (err u109))

(define-data-var next-book-id uint u1)
(define-data-var platform-fee uint u250)

(define-map books
  { book-id: uint }
  {
    title: (string-ascii 100),
    author: principal,
    price: uint,
    royalty-rate: uint,
    total-sales: uint,
    total-earnings: uint,
    published-at: uint,
    is-published: bool,
    content-hash: (string-ascii 64)
  }
)

(define-map purchases
  { buyer: principal, book-id: uint }
  {
    purchased-at: uint,
    amount-paid: uint
  }
)

(define-map author-stats
  { author: principal }
  {
    total-books: uint,
    total-earnings: uint,
    total-sales: uint
  }
)

(define-map book-reviews
  { book-id: uint, reviewer: principal }
  {
    rating: uint,
    review-text: (string-ascii 500),
    reviewed-at: uint
  }
)

(define-map book-ratings
  { book-id: uint }
  {
    total-ratings: uint,
    average-rating: uint,
    total-score: uint
  }
)

(define-public (publish-book (title (string-ascii 100)) (price uint) (royalty-rate uint) (content-hash (string-ascii 64)))
  (let (
    (book-id (var-get next-book-id))
    (current-height stacks-block-height)
  )
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (<= royalty-rate u10000) ERR_INVALID_ROYALTY)
    (asserts! (is-none (map-get? books { book-id: book-id })) ERR_BOOK_ALREADY_EXISTS)
    
    (map-set books
      { book-id: book-id }
      {
        title: title,
        author: tx-sender,
        price: price,
        royalty-rate: royalty-rate,
        total-sales: u0,
        total-earnings: u0,
        published-at: current-height,
        is-published: true,
        content-hash: content-hash
      }
    )
    
    (let ((current-stats (default-to { total-books: u0, total-earnings: u0, total-sales: u0 } 
                          (map-get? author-stats { author: tx-sender }))))
      (map-set author-stats
        { author: tx-sender }
        {
          total-books: (+ (get total-books current-stats) u1),
          total-earnings: (get total-earnings current-stats),
          total-sales: (get total-sales current-stats)
        }
      )
    )
    
    (var-set next-book-id (+ book-id u1))
    (ok book-id)
  )
)

(define-public (purchase-book (book-id uint))
  (let (
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (buyer tx-sender)
    (price (get price book))
    (author (get author book))
    (royalty-rate (get royalty-rate book))
    (platform-fee-rate (var-get platform-fee))
    (current-height stacks-block-height)
  )
    (asserts! (get is-published book) ERR_BOOK_NOT_PUBLISHED)
    (asserts! (is-none (map-get? purchases { buyer: buyer, book-id: book-id })) ERR_PURCHASE_ALREADY_EXISTS)
    
    (let (
      (platform-fee-amount (/ (* price platform-fee-rate) u10000))
      (author-earnings (- price platform-fee-amount))
    )
      (try! (stx-transfer? price buyer (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? author-earnings tx-sender author)))
      
      (map-set purchases
        { buyer: buyer, book-id: book-id }
        {
          purchased-at: current-height,
          amount-paid: price
        }
      )
      
      (map-set books
        { book-id: book-id }
        (merge book {
          total-sales: (+ (get total-sales book) u1),
          total-earnings: (+ (get total-earnings book) author-earnings)
        })
      )
      
      (let ((author-current-stats (default-to { total-books: u0, total-earnings: u0, total-sales: u0 } 
                                   (map-get? author-stats { author: author }))))
        (map-set author-stats
          { author: author }
          {
            total-books: (get total-books author-current-stats),
            total-earnings: (+ (get total-earnings author-current-stats) author-earnings),
            total-sales: (+ (get total-sales author-current-stats) u1)
          }
        )
      )
      
      (ok true)
    )
  )
)

(define-public (add-review (book-id uint) (rating uint) (review-text (string-ascii 500)))
  (let (
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (reviewer tx-sender)
    (current-height stacks-block-height)
  )
    (asserts! (get is-published book) ERR_BOOK_NOT_PUBLISHED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-some (map-get? purchases { buyer: reviewer, book-id: book-id })) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? book-reviews { book-id: book-id, reviewer: reviewer })) ERR_REVIEW_ALREADY_EXISTS)
    
    (map-set book-reviews
      { book-id: book-id, reviewer: reviewer }
      {
        rating: rating,
        review-text: review-text,
        reviewed-at: current-height
      }
    )
    
    (let ((current-ratings (default-to { total-ratings: u0, average-rating: u0, total-score: u0 } 
                           (map-get? book-ratings { book-id: book-id }))))
      (let (
        (new-total-ratings (+ (get total-ratings current-ratings) u1))
        (new-total-score (+ (get total-score current-ratings) rating))
      )
        (map-set book-ratings
          { book-id: book-id }
          {
            total-ratings: new-total-ratings,
            average-rating: (/ new-total-score new-total-ratings),
            total-score: new-total-score
          }
        )
      )
    )
    
    (ok true)
  )
)

(define-public (update-book-price (book-id uint) (new-price uint))
  (let (
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get author book)) ERR_UNAUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_PRICE)
    
    (map-set books
      { book-id: book-id }
      (merge book { price: new-price })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_ROYALTY)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-book (book-id uint))
  (map-get? books { book-id: book-id })
)

(define-read-only (get-purchase (buyer principal) (book-id uint))
  (map-get? purchases { buyer: buyer, book-id: book-id })
)

(define-read-only (has-purchased (buyer principal) (book-id uint))
  (is-some (map-get? purchases { buyer: buyer, book-id: book-id }))
)

(define-read-only (get-author-stats (author principal))
  (map-get? author-stats { author: author })
)

(define-read-only (get-book-review (book-id uint) (reviewer principal))
  (map-get? book-reviews { book-id: book-id, reviewer: reviewer })
)

(define-read-only (get-book-ratings (book-id uint))
  (map-get? book-ratings { book-id: book-id })
)

(define-read-only (get-next-book-id)
  (var-get next-book-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-public (withdraw-platform-fees)
  (let (
    (balance (stx-get-balance (as-contract tx-sender)))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> balance u0) ERR_INSUFFICIENT_PAYMENT)
    (as-contract (stx-transfer? balance tx-sender CONTRACT_OWNER))
  )
)
