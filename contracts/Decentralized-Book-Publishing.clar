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
(define-constant ERR_INVALID_DISCOUNT (err u110))
(define-constant ERR_DISCOUNT_EXPIRED (err u111))
(define-constant ERR_COLLECTION_NOT_FOUND (err u112))
(define-constant ERR_COLLECTION_ALREADY_EXISTS (err u113))
(define-constant ERR_BOOK_ALREADY_IN_COLLECTION (err u114))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u115))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u116))
(define-constant ERR_PLAN_NOT_FOUND (err u117))
(define-constant ERR_PLAN_NOT_ACTIVE (err u118))
(define-constant ERR_INVALID_DURATION (err u119))
(define-constant ERR_SUBSCRIPTION_EXISTS (err u120))

(define-data-var next-book-id uint u1)
(define-data-var platform-fee uint u250)
(define-data-var next-collection-id uint u1)
(define-data-var next-subscription-plan-id uint u1)

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

(define-map book-discounts
  { book-id: uint }
  {
    discount-percentage: uint,
    start-height: uint,
    end-height: uint,
    is-active: bool
  }
)

(define-map book-collections
  { collection-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    created-at: uint,
    total-books: uint,
    is-active: bool
  }
)

(define-map collection-books
  { collection-id: uint, book-id: uint }
  {
    added-at: uint
  }
)

(define-map book-collection-membership
  { book-id: uint }
  {
    collection-id: uint,
    position: uint
  }
)

(define-map subscription-plans
  { plan-id: uint }
  {
    author: principal,
    plan-name: (string-ascii 100),
    monthly-price: uint,
    yearly-price: uint,
    max-books: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-map user-subscriptions
  { subscriber: principal, author: principal }
  {
    plan-id: uint,
    subscribed-at: uint,
    expires-at: uint,
    total-paid: uint,
    is-active: bool
  }
)

(define-map reading-progress
  { reader: principal, book-id: uint }
  {
    pages-read: uint,
    total-pages: uint,
    last-read-at: uint,
    reading-sessions: uint,
    completion-percentage: uint
  }
)

(define-map subscription-access
  { subscriber: principal, book-id: uint }
  {
    granted-at: uint,
    access-type: (string-ascii 20)
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
    (base-price (get price book))
    (author (get author book))
    (royalty-rate (get royalty-rate book))
    (platform-fee-rate (var-get platform-fee))
    (current-height stacks-block-height)
    (discount-info (map-get? book-discounts { book-id: book-id }))
  )
    (asserts! (get is-published book) ERR_BOOK_NOT_PUBLISHED)
    (asserts! (is-none (map-get? purchases { buyer: buyer, book-id: book-id })) ERR_PURCHASE_ALREADY_EXISTS)
    
    (let (
      (final-price (match discount-info
        discount-data (if (and (get is-active discount-data)
                              (>= current-height (get start-height discount-data))
                              (<= current-height (get end-height discount-data)))
                         (- base-price (/ (* base-price (get discount-percentage discount-data)) u10000))
                         base-price)
        base-price))
      (platform-fee-amount (/ (* final-price platform-fee-rate) u10000))
      (author-earnings (- final-price platform-fee-amount))
    )
      (try! (stx-transfer? final-price buyer (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? author-earnings tx-sender author)))
      
      (map-set purchases
        { buyer: buyer, book-id: book-id }
        {
          purchased-at: current-height,
          amount-paid: final-price
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

(define-public (set-book-discount (book-id uint) (discount-percentage uint) (duration-blocks uint))
  (let (
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (current-height stacks-block-height)
    (end-height (+ current-height duration-blocks))
  )
    (asserts! (is-eq tx-sender (get author book)) ERR_UNAUTHORIZED)
    (asserts! (<= discount-percentage u9000) ERR_INVALID_DISCOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_DISCOUNT)
    
    (map-set book-discounts
      { book-id: book-id }
      {
        discount-percentage: discount-percentage,
        start-height: current-height,
        end-height: end-height,
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (deactivate-book-discount (book-id uint))
  (let (
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (discount (unwrap! (map-get? book-discounts { book-id: book-id }) ERR_BOOK_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get author book)) ERR_UNAUTHORIZED)
    
    (map-set book-discounts
      { book-id: book-id }
      (merge discount { is-active: false })
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

(define-read-only (get-book-discount (book-id uint))
  (map-get? book-discounts { book-id: book-id })
)

(define-read-only (get-discounted-price (book-id uint))
  (let (
    (book (map-get? books { book-id: book-id }))
    (discount (map-get? book-discounts { book-id: book-id }))
    (current-height stacks-block-height)
  )
    (match book
      book-data (match discount
        discount-data (if (and (get is-active discount-data)
                              (>= current-height (get start-height discount-data))
                              (<= current-height (get end-height discount-data)))
                         (some (- (get price book-data) (/ (* (get price book-data) (get discount-percentage discount-data)) u10000)))
                         (some (get price book-data)))
        (some (get price book-data)))
      none)
  )
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

(define-public (create-collection (name (string-ascii 100)) (description (string-ascii 500)))
  (let (
    (collection-id (var-get next-collection-id))
    (current-height stacks-block-height)
  )
    (map-set book-collections
      { collection-id: collection-id }
      {
        name: name,
        description: description,
        creator: tx-sender,
        created-at: current-height,
        total-books: u0,
        is-active: true
      }
    )
    (var-set next-collection-id (+ collection-id u1))
    (ok collection-id)
  )
)

(define-public (add-book-to-collection (collection-id uint) (book-id uint))
  (let (
    (collection (unwrap! (map-get? book-collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (current-height stacks-block-height)
    (existing-membership (map-get? book-collection-membership { book-id: book-id }))
  )
    (asserts! (is-eq tx-sender (get creator collection)) ERR_UNAUTHORIZED)
    (asserts! (is-eq tx-sender (get author book)) ERR_UNAUTHORIZED)
    (asserts! (get is-active collection) ERR_COLLECTION_NOT_FOUND)
    (asserts! (is-none existing-membership) ERR_BOOK_ALREADY_IN_COLLECTION)
    
    (map-set collection-books
      { collection-id: collection-id, book-id: book-id }
      {
        added-at: current-height
      }
    )
    
    (map-set book-collection-membership
      { book-id: book-id }
      {
        collection-id: collection-id,
        position: (get total-books collection)
      }
    )
    
    (map-set book-collections
      { collection-id: collection-id }
      (merge collection {
        total-books: (+ (get total-books collection) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (remove-book-from-collection (collection-id uint) (book-id uint))
  (let (
    (collection (unwrap! (map-get? book-collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
    (membership (unwrap! (map-get? book-collection-membership { book-id: book-id }) ERR_BOOK_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get creator collection)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get collection-id membership) collection-id) ERR_BOOK_NOT_FOUND)
    
    (map-delete collection-books { collection-id: collection-id, book-id: book-id })
    (map-delete book-collection-membership { book-id: book-id })
    
    (map-set book-collections
      { collection-id: collection-id }
      (merge collection {
        total-books: (- (get total-books collection) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (purchase-collection (collection-id uint))
  (let (
    (collection (unwrap! (map-get? book-collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    (buyer tx-sender)
  )
    (asserts! (get is-active collection) ERR_COLLECTION_NOT_FOUND)
    (asserts! (> (get total-books collection) u0) ERR_BOOK_NOT_FOUND)
    (ok collection-id)
  )
)

(define-public (toggle-collection-status (collection-id uint))
  (let (
    (collection (unwrap! (map-get? book-collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get creator collection)) ERR_UNAUTHORIZED)
    
    (map-set book-collections
      { collection-id: collection-id }
      (merge collection {
        is-active: (not (get is-active collection))
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-collection (collection-id uint))
  (map-get? book-collections { collection-id: collection-id })
)

(define-read-only (get-book-collection-info (book-id uint))
  (map-get? book-collection-membership { book-id: book-id })
)

(define-read-only (is-book-in-collection (collection-id uint) (book-id uint))
  (is-some (map-get? collection-books { collection-id: collection-id, book-id: book-id }))
)

(define-read-only (get-next-collection-id)
  (var-get next-collection-id)
)

(define-public (create-subscription-plan 
  (plan-name (string-ascii 100))
  (monthly-price uint)
  (yearly-price uint)
  (max-books uint))
  (let
    (
      (plan-id (var-get next-subscription-plan-id))
      (current-height stacks-block-height)
    )
    (asserts! (> monthly-price u0) ERR_INVALID_PRICE)
    (asserts! (> yearly-price u0) ERR_INVALID_PRICE)
    (asserts! (> max-books u0) ERR_INVALID_DURATION)
    (asserts! (< yearly-price (* monthly-price u12)) ERR_INVALID_PRICE)
    
    (map-set subscription-plans
      { plan-id: plan-id }
      {
        author: tx-sender,
        plan-name: plan-name,
        monthly-price: monthly-price,
        yearly-price: yearly-price,
        max-books: max-books,
        created-at: current-height,
        is-active: true
      }
    )
    (var-set next-subscription-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (toggle-subscription-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get author plan)) ERR_UNAUTHORIZED)
    
    (map-set subscription-plans
      { plan-id: plan-id }
      (merge plan { is-active: (not (get is-active plan)) })
    )
    (ok true)
  )
)

(define-public (update-subscription-pricing 
  (plan-id uint)
  (new-monthly-price uint)
  (new-yearly-price uint))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get author plan)) ERR_UNAUTHORIZED)
    (asserts! (> new-monthly-price u0) ERR_INVALID_PRICE)
    (asserts! (> new-yearly-price u0) ERR_INVALID_PRICE)
    (asserts! (< new-yearly-price (* new-monthly-price u12)) ERR_INVALID_PRICE)
    
    (map-set subscription-plans
      { plan-id: plan-id }
      (merge plan {
        monthly-price: new-monthly-price,
        yearly-price: new-yearly-price
      })
    )
    (ok true)
  )
)

(define-public (subscribe-to-author 
  (plan-id uint)
  (is-yearly bool))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (author (get author plan))
      (subscriber tx-sender)
      (current-height stacks-block-height)
      (subscription-price (if is-yearly (get yearly-price plan) (get monthly-price plan)))
      (duration-blocks (if is-yearly u52560 u4380))
      (expires-at (+ current-height duration-blocks))
      (platform-fee-amount (/ (* subscription-price (var-get platform-fee)) u10000))
      (author-earnings (- subscription-price platform-fee-amount))
      (existing-subscription (map-get? user-subscriptions { subscriber: subscriber, author: author }))
    )
    (asserts! (get is-active plan) ERR_PLAN_NOT_ACTIVE)
    (asserts! (is-none existing-subscription) ERR_SUBSCRIPTION_EXISTS)
    
    (try! (stx-transfer? subscription-price subscriber (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? author-earnings tx-sender author)))
    
    (map-set user-subscriptions
      { subscriber: subscriber, author: author }
      {
        plan-id: plan-id,
        subscribed-at: current-height,
        expires-at: expires-at,
        total-paid: subscription-price,
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (renew-subscription 
  (author principal)
  (is-yearly bool))
  (let
    (
      (subscriber tx-sender)
      (current-height stacks-block-height)
      (existing-subscription (unwrap! (map-get? user-subscriptions { subscriber: subscriber, author: author }) ERR_SUBSCRIPTION_NOT_FOUND))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id existing-subscription) }) ERR_PLAN_NOT_FOUND))
      (subscription-price (if is-yearly (get yearly-price plan) (get monthly-price plan)))
      (duration-blocks (if is-yearly u52560 u4380))
      (new-expires-at (+ current-height duration-blocks))
      (platform-fee-amount (/ (* subscription-price (var-get platform-fee)) u10000))
      (author-earnings (- subscription-price platform-fee-amount))
    )
    (asserts! (get is-active plan) ERR_PLAN_NOT_ACTIVE)
    
    (try! (stx-transfer? subscription-price subscriber (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? author-earnings tx-sender author)))
    
    (map-set user-subscriptions
      { subscriber: subscriber, author: author }
      (merge existing-subscription {
        expires-at: new-expires-at,
        total-paid: (+ (get total-paid existing-subscription) subscription-price),
        is-active: true
      })
    )
    (ok true)
  )
)

(define-public (cancel-subscription (author principal))
  (let
    (
      (subscriber tx-sender)
      (existing-subscription (unwrap! (map-get? user-subscriptions { subscriber: subscriber, author: author }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (get is-active existing-subscription) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (map-set user-subscriptions
      { subscriber: subscriber, author: author }
      (merge existing-subscription { is-active: false })
    )
    (ok true)
  )
)

(define-public (start-reading-session 
  (book-id uint)
  (total-pages uint))
  (let
    (
      (reader tx-sender)
      (book (unwrap! (map-get? books { book-id: book-id }) ERR_BOOK_NOT_FOUND))
      (author (get author book))
      (current-height stacks-block-height)
      (has-purchased (is-some (map-get? purchases { buyer: reader, book-id: book-id })))
      (has-subscription (match (map-get? user-subscriptions { subscriber: reader, author: author })
        sub-info (and (get is-active sub-info) (< current-height (get expires-at sub-info)))
        false))
      (existing-progress (map-get? reading-progress { reader: reader, book-id: book-id }))
    )
    (asserts! (get is-published book) ERR_BOOK_NOT_PUBLISHED)
    (asserts! (or has-purchased has-subscription) ERR_UNAUTHORIZED)
    (asserts! (> total-pages u0) ERR_INVALID_DURATION)
    
    (match existing-progress
      progress-data
        (map-set reading-progress
          { reader: reader, book-id: book-id }
          (merge progress-data {
            total-pages: total-pages,
            last-read-at: current-height,
            reading-sessions: (+ (get reading-sessions progress-data) u1)
          })
        )
      (map-set reading-progress
        { reader: reader, book-id: book-id }
        {
          pages-read: u0,
          total-pages: total-pages,
          last-read-at: current-height,
          reading-sessions: u1,
          completion-percentage: u0
        }
      )
    )
    
    (if has-subscription
      (map-set subscription-access
        { subscriber: reader, book-id: book-id }
        {
          granted-at: current-height,
          access-type: "subscription"
        }
      )
      true
    )
    (ok true)
  )
)

(define-public (update-reading-progress 
  (book-id uint)
  (pages-read uint))
  (let
    (
      (reader tx-sender)
      (current-height stacks-block-height)
      (existing-progress (unwrap! (map-get? reading-progress { reader: reader, book-id: book-id }) ERR_BOOK_NOT_FOUND))
      (total-pages (get total-pages existing-progress))
      (completion-percentage (if (> total-pages u0) (/ (* pages-read u100) total-pages) u0))
    )
    (asserts! (<= pages-read total-pages) ERR_INVALID_DURATION)
    
    (map-set reading-progress
      { reader: reader, book-id: book-id }
      (merge existing-progress {
        pages-read: pages-read,
        last-read-at: current-height,
        completion-percentage: completion-percentage
      })
    )
    (ok true)
  )
)

(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans { plan-id: plan-id })
)

(define-read-only (get-user-subscription (subscriber principal) (author principal))
  (map-get? user-subscriptions { subscriber: subscriber, author: author })
)

(define-read-only (get-reading-progress (reader principal) (book-id uint))
  (map-get? reading-progress { reader: reader, book-id: book-id })
)

(define-read-only (get-subscription-access (subscriber principal) (book-id uint))
  (map-get? subscription-access { subscriber: subscriber, book-id: book-id })
)

(define-read-only (has-active-subscription (subscriber principal) (author principal))
  (match (map-get? user-subscriptions { subscriber: subscriber, author: author })
    sub-info
      (and 
        (get is-active sub-info)
        (< stacks-block-height (get expires-at sub-info))
      )
    false
  )
)

(define-read-only (can-access-book (reader principal) (book-id uint))
  (let
    (
      (book (map-get? books { book-id: book-id }))
      (has-purchased (is-some (map-get? purchases { buyer: reader, book-id: book-id })))
    )
    (match book
      book-data
        (let
          (
            (author (get author book-data))
            (has-subscription (has-active-subscription reader author))
          )
          (or has-purchased has-subscription)
        )
      false
    )
  )
)

(define-read-only (get-subscription-status (subscriber principal) (author principal))
  (match (map-get? user-subscriptions { subscriber: subscriber, author: author })
    sub-info
      (let
        (
          (current-height stacks-block-height)
          (is-expired (>= current-height (get expires-at sub-info)))
          (blocks-remaining (if is-expired u0 (- (get expires-at sub-info) current-height)))
        )
        (some {
          plan-id: (get plan-id sub-info),
          is-active: (get is-active sub-info),
          expires-at: (get expires-at sub-info),
          is-expired: is-expired,
          blocks-remaining: blocks-remaining,
          total-paid: (get total-paid sub-info)
        })
      )
    none
  )
)

(define-read-only (get-next-subscription-plan-id)
  (var-get next-subscription-plan-id)
)
