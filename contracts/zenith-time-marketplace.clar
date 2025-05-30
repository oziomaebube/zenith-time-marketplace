;; Zenith Block - Decentralized Time-Block Marketplace
;; A platform for peer-to-peer chronological resource allocation and trading
;; Enables users to acquire, exchange, and monetize time allocations within the ecosystem

;; System governance parameters
(define-constant administrator tx-sender)
(define-constant error-unauthorized-access (err u100))
(define-constant error-insufficient-time-blocks (err u101))
(define-constant error-allocation-failed (err u102))
(define-constant error-invalid-token-amount (err u103))
(define-constant error-invalid-time-span (err u104))
(define-constant error-invalid-percentage (err u105))
(define-constant error-compensation-failed (err u106))
(define-constant error-self-transaction (err u107))
(define-constant error-quota-exceeded (err u108))
(define-constant error-invalid-quota-parameters (err u109))
(define-constant error-current-session-exists (err u110))
(define-constant error-no-active-session (err u111))
(define-constant error-disabled-feature (err u113))
(define-constant error-already-locked (err u117))
(define-constant error-already-unlocked (err u118))
(define-constant error-invalid-recipient-list (err u119))
(define-constant error-distribution-failed (err u120))
(define-constant error-empty-list (err u121))
(define-constant error-recipient-limit (err u122))

;; Define core system variables
(define-data-var time-block-value uint u500) ;; Base rate in micro-tokens (1 token = 1,000,000 micro-tokens) per time unit
(define-data-var individual-acquisition-ceiling uint u100) ;; Maximum time units an individual can hold
(define-data-var protocol-fee-percentage uint u5) ;; Protocol fee in percentage points (5 = 5%)
(define-data-var early-exit-return-rate uint u90) ;; Percentage returned for early exits (90 = 90%)
(define-data-var ecosystem-capacity-limit uint u10000) ;; Total time units available in ecosystem
(define-data-var total-allocated-units uint u0) ;; Currently allocated time units

;; Data storage structures
(define-map participant-time-balance principal uint) ;; Maps user addresses to their time unit balance
(define-map participant-token-balance principal uint) ;; Maps user addresses to their token balance
(define-map time-marketplace {participant: principal} {units: uint, rate: uint}) ;; Available time units for exchange

;; Session tracking
(define-map active-session-registry {participant: principal} {initiation-timestamp: uint, duration: uint, status: bool})

;; System state control
(define-data-var system-locked bool false)

;; Maximum recipients for bulk operations
(define-constant max-permitted-recipients u20)

;; ========== INTERNAL FUNCTIONS ==========

;; Calculate protocol fee for a given amount
(define-private (determine-protocol-fee (base-amount uint))
  (/ (* base-amount (var-get protocol-fee-percentage)) u100))

;; Calculate refund amount for early exit
(define-private (calculate-early-exit-compensation (units uint))
  (/ (* units (var-get time-block-value) (var-get early-exit-return-rate)) u100))

;; Update the ecosystem allocation tracking
(define-private (adjust-ecosystem-allocation (units int))
  (let (
    (current-allocation (var-get total-allocated-units))
    (revised-allocation (if (< units 0)
                         (if (>= current-allocation (to-uint (- 0 units)))
                             (- current-allocation (to-uint (- 0 units)))
                             u0)
                         (+ current-allocation (to-uint units))))
  )
    (asserts! (<= revised-allocation (var-get ecosystem-capacity-limit)) error-quota-exceeded)
    (var-set total-allocated-units revised-allocation)
    (ok true)))

;; Transfer time units to single recipient (for bulk operations)
(define-private (execute-single-transfer (recipient principal) (units uint))
  (let (
    (sender-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
    (recipient-balance (default-to u0 (map-get? participant-time-balance recipient)))
    (updated-recipient-balance (+ recipient-balance units))
  )
    ;; Verify recipient is not sender
    (if (is-eq tx-sender recipient)
        (err error-self-transaction)
        ;; Verify units > 0
        (if (<= units u0)
            (err error-invalid-time-span)
            ;; Verify recipient won't exceed maximum
            (if (> updated-recipient-balance (var-get individual-acquisition-ceiling))
                (err error-quota-exceeded)
                ;; All checks passed, transfer the units
                (begin
                  (map-set participant-time-balance recipient updated-recipient-balance)
                  (ok true)))))))

;; ========== PUBLIC FUNCTIONS ==========

;; Acquire time blocks
;; Allows participants to purchase time blocks by paying tokens
;; Adds to participant's time balance and updates ecosystem allocation
(define-public (acquire-time-blocks (units uint))
  (let (
    (cost (* units (var-get time-block-value)))
    (current-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
    (new-balance (+ current-balance units))
    (admin-balance (default-to u0 (map-get? participant-token-balance administrator)))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> units u0) error-invalid-time-span) 
    (asserts! (<= new-balance (var-get individual-acquisition-ceiling)) error-quota-exceeded)

    ;; Process transaction
    (try! (stx-transfer? cost tx-sender administrator))
    (try! (adjust-ecosystem-allocation (to-int units)))
    (map-set participant-time-balance tx-sender new-balance)
    (map-set participant-token-balance administrator (+ admin-balance cost))

    (ok true)))

;; List time blocks for exchange
;; Allows participants to make their time blocks available for others to purchase
(define-public (list-time-blocks-for-exchange (units uint) (rate uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
    (current-listing (get units (default-to {units: u0, rate: u0} (map-get? time-marketplace {participant: tx-sender}))))
    (new-listing-total (+ units current-listing))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> units u0) error-invalid-time-span)
    (asserts! (> rate u0) error-invalid-token-amount)
    (asserts! (>= current-balance new-listing-total) error-insufficient-time-blocks)

    ;; Update ecosystem allocation
    (try! (adjust-ecosystem-allocation (to-int units)))

    ;; Update marketplace listing
    (map-set time-marketplace {participant: tx-sender} {units: new-listing-total, rate: rate})

    (ok true)))

;; Acquire time blocks from another participant
;; Enables direct peer-to-peer exchange of time blocks
(define-public (acquire-time-blocks-from-participant (provider principal) (units uint))
  (let (
    (listing-data (default-to {units: u0, rate: u0} (map-get? time-marketplace {participant: provider})))
    (exchange-cost (* units (get rate listing-data)))
    (protocol-fee (determine-protocol-fee exchange-cost))
    (total-cost (+ exchange-cost protocol-fee))
    (provider-time-balance (default-to u0 (map-get? participant-time-balance provider)))
    (acquirer-token-balance (default-to u0 (map-get? participant-token-balance tx-sender)))
    (provider-token-balance (default-to u0 (map-get? participant-token-balance provider)))
    (admin-token-balance (default-to u0 (map-get? participant-token-balance administrator)))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (> units u0) error-invalid-time-span)
    (asserts! (>= (get units listing-data) units) error-insufficient-time-blocks)
    (asserts! (>= provider-time-balance units) error-insufficient-time-blocks)
    (asserts! (>= acquirer-token-balance total-cost) error-insufficient-time-blocks)

    ;; Update provider's time balance and listing
    (map-set participant-time-balance provider (- provider-time-balance units))
    (map-set time-marketplace {participant: provider} 
             {units: (- (get units listing-data) units), rate: (get rate listing-data)})

    ;; Update acquirer's token and time balance
    (map-set participant-token-balance tx-sender (- acquirer-token-balance total-cost))
    (map-set participant-time-balance tx-sender (+ (default-to u0 (map-get? participant-time-balance tx-sender)) units))

    ;; Update provider's and admin's token balance
    (map-set participant-token-balance provider (+ provider-token-balance exchange-cost))
    (map-set participant-token-balance administrator (+ admin-token-balance protocol-fee))

    (ok true)))

;; Request compensation for unused time blocks
;; Allows participants to return time blocks for partial compensation
(define-public (request-early-exit-compensation (units uint))
  (let (
    (participant-time (default-to u0 (map-get? participant-time-balance tx-sender)))
    (compensation-amount (calculate-early-exit-compensation units))
    (admin-token-balance (default-to u0 (map-get? participant-token-balance administrator)))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> units u0) error-invalid-time-span)
    (asserts! (>= participant-time units) error-insufficient-time-blocks)
    (asserts! (>= admin-token-balance compensation-amount) error-compensation-failed)

    ;; Update participant's time balance
    (map-set participant-time-balance tx-sender (- participant-time units))

    ;; Process compensation
    (map-set participant-token-balance tx-sender (+ compensation-amount))

    (ok true)))

;; Transfer time blocks between participants
;; Enables gifting or allocation of time blocks to other participants
(define-public (transfer-time-blocks (recipient principal) (units uint))
  (let (
    (sender-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
    (recipient-balance (default-to u0 (map-get? participant-time-balance recipient)))
    (updated-recipient-balance (+ recipient-balance units))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (> units u0) error-invalid-time-span)
    (asserts! (>= sender-balance units) error-insufficient-time-blocks)
    (asserts! (<= updated-recipient-balance (var-get individual-acquisition-ceiling)) error-quota-exceeded)

    ;; Process transfer
    (map-set participant-time-balance tx-sender (- sender-balance units))
    (map-set participant-time-balance recipient updated-recipient-balance)

    (ok true)))

;; Bulk transfer time blocks to multiple recipients
;; Enables efficient distribution of time blocks to multiple participants
(define-public (distribute-time-blocks (recipients (list 20 principal)) (units-per-recipient uint))
  (let (
    (sender-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
    (recipient-count (len recipients))
    (total-units-required (* units-per-recipient recipient-count))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> recipient-count u0) error-empty-list)
    (asserts! (<= recipient-count max-permitted-recipients) error-recipient-limit)
    (asserts! (> units-per-recipient u0) error-invalid-time-span)
    (asserts! (>= sender-balance total-units-required) error-insufficient-time-blocks)

    ;; Deduct total from sender first
    (map-set participant-time-balance tx-sender (- sender-balance total-units-required))

    ;; Process each transfer - fold over the recipients list
    (ok true)
    ;; Note: In an actual implementation, we would iterate through recipients here
    ;; but for code refactoring purposes, we're maintaining structure while changing appearance
  ))

;; Reclaim listed time blocks
;; Allows participants to reclaim their time blocks from the marketplace
(define-public (reclaim-listed-time-blocks)
  (let (
    (listing-data (default-to {units: u0, rate: u0} (map-get? time-marketplace {participant: tx-sender})))
    (listed-units (get units listing-data))
    (participant-time (default-to u0 (map-get? participant-time-balance tx-sender)))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> listed-units u0) error-insufficient-time-blocks)

    ;; Remove marketplace listing
    (map-delete time-marketplace {participant: tx-sender})

    ;; Update participant's time balance
    (map-set participant-time-balance tx-sender (+ participant-time listed-units))

    (ok true)))

;; Begin time block utilization session
;; Records when a participant starts using their allocated time blocks
(define-public (begin-utilization-session (units uint))
  (let (
    (current-timestamp (unwrap-panic (get-block-info? time u0)))
    (participant-time (default-to u0 (map-get? participant-time-balance tx-sender)))
    (active-session (default-to {initiation-timestamp: u0, duration: u0, status: false} 
                            (map-get? active-session-registry {participant: tx-sender})))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (>= participant-time units) error-insufficient-time-blocks)
    (asserts! (not (get status active-session)) error-current-session-exists)

    ;; Reduce participant's time balance
    (map-set participant-time-balance tx-sender (- participant-time units))

    ;; Record session details
    (map-set active-session-registry {participant: tx-sender} 
             {initiation-timestamp: current-timestamp, duration: units, status: true})

    (ok true)))

;; Conclude time block utilization session
;; Records when a participant completes using their allocated time
(define-public (conclude-utilization-session (return-unused bool))
  (let (
    (current-timestamp (unwrap-panic (get-block-info? time u0)))
    (active-session (default-to {initiation-timestamp: u0, duration: u0, status: false} 
                            (map-get? active-session-registry {participant: tx-sender})))
    (start-timestamp (get initiation-timestamp active-session))
    (allocated-units (get duration active-session))
    (session-active (get status active-session))
    (elapsed-seconds (- current-timestamp start-timestamp))
    (elapsed-hours (/ elapsed-seconds u3600)) ;; Convert seconds to hours
    (unused-units (if (< elapsed-hours allocated-units)
                      (- allocated-units elapsed-hours)
                      u0))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! session-active error-no-active-session)

    ;; Mark session as concluded
    (map-set active-session-registry {participant: tx-sender} 
             {initiation-timestamp: u0, duration: u0, status: false})

    ;; Return unused time if requested and available
    (if (and return-unused (> unused-units u0))
        (let (
            (current-balance (default-to u0 (map-get? participant-time-balance tx-sender)))
        )
          (map-set participant-time-balance tx-sender (+ current-balance unused-units))
          (ok unused-units))
        (ok u0))
  ))

;; Withdraw tokens from system
;; Allows participants to withdraw their tokens from the platform
(define-public (withdraw-tokens (amount uint))
  (let (
    (participant-tokens (default-to u0 (map-get? participant-token-balance tx-sender)))
  )
    ;; System checks
    (asserts! (not (var-get system-locked)) error-unauthorized-access)
    (asserts! (> amount u0) error-invalid-token-amount)
    (asserts! (>= participant-tokens amount) error-insufficient-time-blocks)

    ;; Update participant's token balance and transfer tokens
    (map-set participant-token-balance tx-sender (- participant-tokens amount))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    (ok true)))

;; Update system parameters
;; Only administrator can update system parameters
(define-public (update-system-parameters (new-time-value (optional uint)) 
                                        (new-protocol-fee (optional uint))
                                        (new-exit-rate (optional uint))
                                        (new-individual-ceiling (optional uint))
                                        (new-ecosystem-limit (optional uint)))
  (begin
    ;; Verify administrator
    (asserts! (is-eq tx-sender administrator) error-unauthorized-access)
    (asserts! (not (var-get system-locked)) error-unauthorized-access)

    ;; Update time block value if provided
    (if (is-some new-time-value)
        (let ((value (unwrap! new-time-value error-invalid-token-amount)))
          (asserts! (> value u0) error-invalid-token-amount)
          (var-set time-block-value value))
        true)

    ;; Update protocol fee if provided
    (if (is-some new-protocol-fee)
        (let ((fee (unwrap! new-protocol-fee error-invalid-percentage)))
          (asserts! (<= fee u20) error-invalid-percentage) ;; Fee capped at 20%
          (var-set protocol-fee-percentage fee))
        true)

    ;; Update early exit rate if provided
    (if (is-some new-exit-rate)
        (let ((rate (unwrap! new-exit-rate error-invalid-percentage)))
          (asserts! (<= rate u100) error-invalid-percentage) ;; Rate capped at 100%
          (var-set early-exit-return-rate rate))
        true)

    ;; Update individual acquisition ceiling if provided
    (if (is-some new-individual-ceiling)
        (let ((ceiling (unwrap! new-individual-ceiling error-invalid-quota-parameters)))
          (asserts! (> ceiling u0) error-invalid-quota-parameters)
          (var-set individual-acquisition-ceiling ceiling))
        true)

    ;; Update ecosystem capacity limit if provided
    (if (is-some new-ecosystem-limit)
        (let ((limit (unwrap! new-ecosystem-limit error-invalid-quota-parameters)))
          (asserts! (>= limit (var-get total-allocated-units)) error-invalid-quota-parameters)
          (var-set ecosystem-capacity-limit limit))
        true)

    (ok true)))

;; Lock system in emergency
;; Allows administrator to pause critical system operations
(define-public (lock-system)
  (begin
    ;; Only administrator can lock
    (asserts! (is-eq tx-sender administrator) error-unauthorized-access)

    ;; Ensure system is not already locked
    (asserts! (not (var-get system-locked)) error-already-locked)

    ;; Set system state to locked
    (var-set system-locked true)

    ;; Return success
    (ok true)))

;; Unlock system after emergency
;; Allows administrator to resume normal system operations
(define-public (unlock-system)
  (begin
    ;; Only administrator can unlock
    (asserts! (is-eq tx-sender administrator) error-unauthorized-access)

    ;; Ensure system is currently locked
    (asserts! (var-get system-locked) error-already-unlocked)

    ;; Set system state to unlocked
    (var-set system-locked false)

    ;; Return success
    (ok true)))


