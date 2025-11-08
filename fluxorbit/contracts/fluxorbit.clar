;; FluxOrbit - Decentralized Multi-Chain Domain Registry
;; A dynamic DNS system for dApps with load balancing and failover

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-endpoint (err u104))
(define-constant err-insufficient-payment (err u105))

;; Domain registration fee (in microSTX)
(define-constant registration-fee u1000000)

;; Data Variables
(define-data-var total-domains uint u0)
(define-data-var validator-threshold uint u3)

;; Domain structure with multi-chain endpoints
(define-map domains
    { domain: (string-ascii 64) }
    {
        owner: principal,
        primary-endpoint: (string-ascii 256),
        blockchain-id: (string-ascii 32),
        created-at: uint,
        updated-at: uint,
        version: uint,
        active: bool
    }
)

;; Chain endpoints for load balancing
(define-map chain-endpoints
    { blockchain-id: (string-ascii 32), domain: (string-ascii 64) }
    {
        endpoint-url: (string-ascii 256),
        priority: uint,
        health-score: uint,
        last-checked: uint,
        gas-cost: uint,
        active: bool
    }
)

;; Validator registry
(define-map validators
    { validator: principal }
    {
        reputation: uint,
        total-validations: uint,
        active: bool
    }
)

;; Health reports from validators
(define-map health-reports
    { blockchain-id: (string-ascii 32), domain: (string-ascii 64), validator: principal }
    {
        health-score: uint,
        reported-at: uint,
        gas-cost: uint
    }
)

;; Subdomain delegation
(define-map subdomains
    { parent-domain: (string-ascii 64), subdomain: (string-ascii 64) }
    {
        owner: principal,
        endpoint: (string-ascii 256),
        active: bool
    }
)

;; Read-only functions

(define-read-only (get-domain (domain (string-ascii 64)))
    (map-get? domains { domain: domain })
)

(define-read-only (get-chain-endpoint (domain (string-ascii 64)) (blockchain-id (string-ascii 32)))
    (map-get? chain-endpoints { blockchain-id: blockchain-id, domain: domain })
)

(define-read-only (get-validator (validator principal))
    (map-get? validators { validator: validator })
)

(define-read-only (get-subdomain (parent (string-ascii 64)) (sub (string-ascii 64)))
    (map-get? subdomains { parent-domain: parent, subdomain: sub })
)

(define-read-only (get-total-domains)
    (ok (var-get total-domains))
)

(define-read-only (is-domain-available (domain (string-ascii 64)))
    (ok (is-none (map-get? domains { domain: domain })))
)

;; Public functions

;; Register a new domain
(define-public (register-domain 
    (domain (string-ascii 64))
    (primary-endpoint (string-ascii 256))
    (blockchain-id (string-ascii 32)))
    (let
        (
            (domain-exists (map-get? domains { domain: domain }))
        )
        (asserts! (is-none domain-exists) err-already-exists)
        (try! (stx-transfer? registration-fee tx-sender contract-owner))
        
        (map-set domains
            { domain: domain }
            {
                owner: tx-sender,
                primary-endpoint: primary-endpoint,
                blockchain-id: blockchain-id,
                created-at: block-height,
                updated-at: block-height,
                version: u1,
                active: true
            }
        )
        
        (map-set chain-endpoints
            { blockchain-id: blockchain-id, domain: domain }
            {
                endpoint-url: primary-endpoint,
                priority: u1,
                health-score: u100,
                last-checked: block-height,
                gas-cost: u0,
                active: true
            }
        )
        
        (var-set total-domains (+ (var-get total-domains) u1))
        (ok true)
    )
)

;; Add additional chain endpoint for load balancing
(define-public (add-chain-endpoint
    (domain (string-ascii 64))
    (blockchain-id (string-ascii 32))
    (endpoint-url (string-ascii 256))
    (priority uint))
    (let
        (
            (domain-data (unwrap! (map-get? domains { domain: domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        (map-set chain-endpoints
            { blockchain-id: blockchain-id, domain: domain }
            {
                endpoint-url: endpoint-url,
                priority: priority,
                health-score: u100,
                last-checked: block-height,
                gas-cost: u0,
                active: true
            }
        )
        (ok true)
    )
)

;; Update domain endpoint
(define-public (update-domain-endpoint
    (domain (string-ascii 64))
    (new-endpoint (string-ascii 256))
    (blockchain-id (string-ascii 32)))
    (let
        (
            (domain-data (unwrap! (map-get? domains { domain: domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        (map-set domains
            { domain: domain }
            (merge domain-data {
                primary-endpoint: new-endpoint,
                blockchain-id: blockchain-id,
                updated-at: block-height,
                version: (+ (get version domain-data) u1)
            })
        )
        (ok true)
    )
)

;; Register as validator
(define-public (register-validator)
    (begin
        (map-set validators
            { validator: tx-sender }
            {
                reputation: u100,
                total-validations: u0,
                active: true
            }
        )
        (ok true)
    )
)

;; Submit health report (validator function)
(define-public (submit-health-report
    (domain (string-ascii 64))
    (blockchain-id (string-ascii 32))
    (health-score uint)
    (gas-cost uint))
    (let
        (
            (validator-data (unwrap! (map-get? validators { validator: tx-sender }) err-unauthorized))
            (endpoint-data (unwrap! (map-get? chain-endpoints { blockchain-id: blockchain-id, domain: domain }) err-not-found))
        )
        (asserts! (get active validator-data) err-unauthorized)
        
        (map-set health-reports
            { blockchain-id: blockchain-id, domain: domain, validator: tx-sender }
            {
                health-score: health-score,
                reported-at: block-height,
                gas-cost: gas-cost
            }
        )
        
        ;; Update endpoint health and gas cost
        (map-set chain-endpoints
            { blockchain-id: blockchain-id, domain: domain }
            (merge endpoint-data {
                health-score: health-score,
                gas-cost: gas-cost,
                last-checked: block-height
            })
        )
        
        ;; Update validator stats
        (map-set validators
            { validator: tx-sender }
            (merge validator-data {
                total-validations: (+ (get total-validations validator-data) u1)
            })
        )
        
        (ok true)
    )
)

;; Create subdomain
(define-public (create-subdomain
    (parent-domain (string-ascii 64))
    (subdomain (string-ascii 64))
    (endpoint (string-ascii 256))
    (subdomain-owner principal))
    (let
        (
            (domain-data (unwrap! (map-get? domains { domain: parent-domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        (map-set subdomains
            { parent-domain: parent-domain, subdomain: subdomain }
            {
                owner: subdomain-owner,
                endpoint: endpoint,
                active: true
            }
        )
        (ok true)
    )
)

;; Toggle domain active status
(define-public (toggle-domain-status (domain (string-ascii 64)))
    (let
        (
            (domain-data (unwrap! (map-get? domains { domain: domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        (map-set domains
            { domain: domain }
            (merge domain-data {
                active: (not (get active domain-data)),
                updated-at: block-height
            })
        )
        (ok true)
    )
)

;; Transfer domain ownership
(define-public (transfer-domain (domain (string-ascii 64)) (new-owner principal))
    (let
        (
            (domain-data (unwrap! (map-get? domains { domain: domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        (map-set domains
            { domain: domain }
            (merge domain-data {
                owner: new-owner,
                updated-at: block-height
            })
        )
        (ok true)
    )
)

;; Admin function to update validator threshold
(define-public (set-validator-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set validator-threshold new-threshold)
        (ok true)
    )
)