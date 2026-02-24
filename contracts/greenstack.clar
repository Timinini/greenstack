;; --------------------------------------------------------
;; GreenStack: Tokenized Carbon Credit Marketplace
;; --------------------------------------------------------
;; Fungible token for carbon credits
;; Features:
;; 1. Minting (only authorized verifiers)
;; 2. Listing and selling credits
;; 3. Buying credits with STX
;; 4. Retiring (burning) credits
;; 5. Transparent ledger for verification
;; --------------------------------------------------------

;; IMPORTANT:
;; - This contract defines its own fungible token "carbon"
;; - If you need SIP-010 compliance, we can add `use-trait`/`impl-trait` and
;;   implement the exact SIP-010 surface.

(define-fungible-token carbon)

;; -------------------------------
;; ERROR CODES
;; -------------------------------
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_INSUFFICIENT_BALANCE u101)
(define-constant ERR_NOT_FOR_SALE u102)
(define-constant ERR_INVALID_AMOUNT u103)
(define-constant ERR_ALREADY_RETIRED u104)
(define-constant ERR_NOT_INITIALIZED u105)
(define-constant ERR_ALREADY_INITIALIZED u106)
(define-constant ERR_STX_TRANSFER_FAILED u200)
(define-constant ERR_FT_TRANSFER_FAILED u201)
(define-constant ERR_FT_MINT_FAILED u202)
(define-constant ERR_FT_BURN_FAILED u203)

;; -------------------------------
;; STATE
;; -------------------------------

;; Owner must be set once after deploy
(define-data-var owner (optional principal) none)

;; Marketplace fee in percent (e.g., 3 = 3%)
(define-data-var marketplace-fee uint u3)

;; Keep a list for readback (optional), and a map for fast membership checks
(define-data-var verifier-list (list 20 principal) (list))
(define-map verifiers { addr: principal } bool)

;; Listings keyed by listing-id (uint)
(define-map listings
  { listing-id: uint }
  { seller: principal, price: uint, amount: uint })

;; Retired credits record keyed by retire-id (uint)
(define-map retired-credits
  { retire-id: uint }
  { retired-by: principal, amount: uint })

;; -------------------------------
;; HELPERS
;; -------------------------------

(define-read-only (get-owner)
  (var-get owner)
)

;; Guard that returns ok true or err
(define-private (assert-owner)
  (match (var-get owner)
    some-owner (if (is-eq tx-sender some-owner) (ok true) (err ERR_UNAUTHORIZED))
    (err ERR_NOT_INITIALIZED)
  )
)

(define-read-only (is-verifier (p principal))
  (default-to false (map-get? verifiers { addr: p }))
)

;; -------------------------------
;; ADMIN FUNCTIONS
;; -------------------------------

;; Must be called once by the deployer (or desired admin) after deployment.
(define-public (init-owner)
  (if (is-none (var-get owner))
    (begin
      (var-set owner (some tx-sender))
      (ok true)
    )
    (err ERR_ALREADY_INITIALIZED)
  )
)

(define-public (set-fee (new-fee uint))
  (begin
    (try! (assert-owner))
    (var-set marketplace-fee new-fee)
    (ok true)
  )
)

(define-public (add-verifier (verifier principal))
  (let ((is-duplicate (default-to false (map-get? verifiers { addr: verifier }))))
    (if is-duplicate
      (begin
        (try! (assert-owner))
        (err ERR_INVALID_AMOUNT)
      )
      (begin
        (try! (assert-owner))
        (map-set verifiers { addr: verifier } true)
        (ok "Verifier added")
      )
    )
  )
)

;; -------------------------------
;; CREDIT MINTING
;; -------------------------------

(define-public (mint-credits (recipient principal) (amount uint) (metadata (string-utf8 256)))
  (if (is-verifier tx-sender)
    (if (> amount u0)
      (match (ft-mint? carbon amount recipient)
        success (ok "Minted credits successfully")
        error (err ERR_FT_MINT_FAILED)
      )
      (err ERR_INVALID_AMOUNT)
    )
    (err ERR_UNAUTHORIZED)
  )
)

;; -------------------------------
;; LISTING FOR SALE (No Escrow)
;; -------------------------------

(define-public (list-credits (listing-id uint) (price uint) (amount uint))
  (if (and (> amount u0) (> price u0) (>= (ft-get-balance carbon tx-sender) amount))
    (begin
      (map-set listings
        { listing-id: listing-id }
        { seller: tx-sender, price: price, amount: amount })
      (ok "Credits listed for sale")
    )
    (if (not (> amount u0))
      (err ERR_INVALID_AMOUNT)
      (if (not (> price u0))
        (err ERR_INVALID_AMOUNT)
        (err ERR_INSUFFICIENT_BALANCE)
      )
    )
  )
)

;; -------------------------------
;; BUYING CREDITS
;; -------------------------------

(define-public (buy-credits (listing-id uint) (amount uint))
  (match (map-get? listings { listing-id: listing-id })
    some-listing
      (let (
            (price (get price some-listing))
            (seller (get seller some-listing))
            (listed-amount (get amount some-listing))
            (total-cost (* amount price))
            (fee (/ (* total-cost (var-get marketplace-fee)) u100))
            (seller-share (- total-cost fee))
           )
        (if (and (is-eq amount listed-amount) (>= (ft-get-balance carbon seller) amount))
          (match (var-get owner)
            owner-principal
              (match (stx-transfer? total-cost tx-sender owner-principal)
                stx-success1
                  (match (stx-transfer? seller-share owner-principal seller)
                    stx-success2
                      (match (ft-transfer? carbon amount seller tx-sender)
                        ft-success
                          (begin
                            (map-delete listings { listing-id: listing-id })
                            (ok "Purchase successful")
                          )
                        ft-error (err ERR_FT_TRANSFER_FAILED)
                      )
                    stx-error2 (err ERR_STX_TRANSFER_FAILED)
                  )
                stx-error1 (err ERR_STX_TRANSFER_FAILED)
              )
            (err ERR_NOT_INITIALIZED)
          )
          (if (not (is-eq amount listed-amount))
            (err ERR_INVALID_AMOUNT)
            (err ERR_INSUFFICIENT_BALANCE)
          )
        )
      )
    (err ERR_NOT_FOR_SALE)
  )
)

;; -------------------------------
;; RETIRING CREDITS
;; -------------------------------

;; Note: retire-id is an arbitrary id chosen by caller to index the retirement record.
(define-public (retire-credits (retire-id uint) (amount uint))
  (if (and (> amount u0) (>= (ft-get-balance carbon tx-sender) amount))
    (match (ft-burn? carbon amount tx-sender)
      burn-success
        (begin
          (map-set retired-credits
            { retire-id: retire-id }
            { retired-by: tx-sender, amount: amount })
          (ok "Credits retired successfully")
        )
      burn-error (err ERR_FT_BURN_FAILED)
    )
    (if (not (> amount u0))
      (err ERR_INVALID_AMOUNT)
      (err ERR_INSUFFICIENT_BALANCE)
    )
  )
)

;; -------------------------------
;; VIEW FUNCTIONS
;; -------------------------------

(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

(define-read-only (get-retired (retire-id uint))
  (map-get? retired-credits { retire-id: retire-id })
)

(define-read-only (get-verifiers)
  (var-get verifier-list)
)

(define-read-only (get-fee)
  (var-get marketplace-fee)
)

(define-read-only (balance-of (who principal))
  (ft-get-balance carbon who)
)