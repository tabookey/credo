;; CREDO model simulation
;; by Zvi Avraham <zvi@nivertech.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compound Interest formula
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;A = P*(1+r/n)^(n*t)
;A/P = (1+r/n)^(n*t)
;A/P = (1+r/365)^(365*1)
;A/P = (1+r/365)^365
;
;(A/P)^(1/365) = 1+r/365
;(A/P)^(1/365)-1 = r/365
;((A/P)^(1/365)-1)*365 = r
;
;
;P*(1+R) = P*(1+x)^365
;1+R = (1+x)^365
;
;(1+R)^(1/365) = 1+x
;(1+R)^(1/365)-1 = x
;x = (1+R)^(1/365)-1
;
;dailyInterest = (1 + annualInterest)^(1/365) - 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deriving daily probability to be slashed from annual probability to be slashed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P(slashed) + P(not_slashed) = 1
; P(not_slashed) = 1 - P(slashed)

;P(S_daily) - probability to be slashed per day
;P(S_annual)  - probability to be slashed per year
;
;(1-P(S_daily))^365 = 1 - P(S_annual)
;1-P(S_daily) = (1 - P(S_annual))^(1/365)
;
;P(S_daily) = 1 - (1 - P(S_annual))^(1/365)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Model inputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RRT% (Reserve Ratio Target) - 100,75,50,25,0
;BPs average annual ROI minus BP’s fees (in %) - 150,100,50,25,12,8,4

; TODO - market-direction
; TODO - investor1-strategy

;["market-direction" -2 -1 0 1 2]

;["investor1-investment-dai" 100000]
;["investor1-strategy" "random" "high-risk" "low-risk"]
;["market-direction" -1 0 1]
;["initial-rrt-pct" 100 75 50 25 0]
;["avg-bp-yield-minus-fees-pct" 4 8 12 25 50 100 150]
;["annual-prob-to-be-slashed-pct 0 0.25 1 3 5]

; at least 3 repetitions

; uncheck "measure on every step"

; 365 days in a year
; 365 * 24 = 8760 ; hours in a year
; 365 * 24 * 60 = 525600 ; minutes in a year


; TODO
; ask investor1 [set label "100K       " set label-color white]

extensions [
  cf
  table
]

;; breeds - Agent Types
breed [investors  investor ]
breed [depositors depositor]
breed [BPs        BP       ] ;; BPs - Block Producers
;; credos: are many-to-many links of type Investor -> BP,
;; each of them represent a loan taken by an investor from DCR and delegated to BP
directed-link-breed [credos credo]


depositors-own [
  wallet-dai           ; depositor's wallet on L1 in DAI
  deposit-dai          ; deposit in L2 in DAI
]

investors-own [
  wallet-dai           ; investor's wallet on L1 in DAI
  deposit-dai          ; deposit in L2 in DAI

  ; TODO - there is no capital-dai - only DET - DET-tokens
  capital-dai          ;
  DET-tokens           ; unstaked and unlocked DET received from initial sale and then maybe followup investments
  ;DET-tokens-staked    ; DET tokens staked

  ;voting-related properties
  suspended?           ;
  voted?               ;
  voted-to-change-RRT? ;

  investor-strategy              ;how investor allocates credos to BPs
  investor-strategy-split-to-BPs ;between how many BPs to split credos?
]

; BPs - Block Producers
; In order to enable Alice and Bob to transact on L2, we must incentivize nodes to maintain and operate the sidechain. Their main incentive for producing a block is earning the transaction fees associated with that specific block in proportion to the size of their stake. This is how it works:
; 1. Miners stake W-DAI in L2 in order to participate in PrivaChain’s block production. They can obtain W-DAI by sending an equivalent amount of DAI to PrivaChain’s smart contract on L1.
; 2. The miner that produces the block receives its associated transaction fees.
; 3. The miner is incentivized to continue operating the sidechain, by producing blocks and winning the transaction fees. If the miner is unresponsive, its staked DAI is slashed.
BPs-own [
  wallet-dai          ; BP's wallet on L1 in DAI
  deposit-dai         ; deposit in L2 in DAI

  ; TODO - staking period ?
  annual-staking-rewards-pct ; staking rewards in % paid by this BP
  daily-staking-rewards-multiplier ; daily compounded staking rewards in 1+r (not %)
  ;management-fee-pct  ; management fee in % taken by BP for their services

  p-slashed-annual ; probability to be slashed per year
  p-slashed-daily  ; probability to be slashed [0..1) per day

  BP-phase            ; current BP phase

  slashed-at          ; when (at which tick) this BP was slashed
  unslash-at          ; when (at which tick) this BP will be unslashed
]

credos-own [
  ;end1 - investor agent who took this credo
  ;end2 - BP to which this credo is delegated
  credo-phase         ; current CREDO phase
  credo-taken-at      ; when (at which tick it was taken)
  credo-due-at        ; when (at which tick) need to be repaid

  delete-after-repayment-at ; when to delete repaid credo link

  DET-tokens-staked   ; DET tokens staked on this credo

  credos-dai          ; loans given to holder of DET-tokens
  interest-dai        ; interest from delegated BPs

  grace-period-due    ; when (at which tick) grace period ends
]

globals [
  ; constants
  TICKS-PER-DAY
  TICKS-PER-YEAR
  BLOCK-TIME-IN-TICKS
  BLOCKS-PER-YEAR

  ; DCR colors
  color-dcr-background
  color-dcr-exsess-reserve
  color-dcr-deposits
  color-dcr-capital

  ; voting colors
  color-investor-non-voting
  color-investor-suspended
  color-investor-voted-yes
  color-investor-voted-no

  dcr-viz-col1-dx ; DCR visualization width
  dcr-viz-col2-dx ; DCR visualization width

  BPs-hidden?
  investor1-hidden?

  DCR-phase
  DCR-phase-init
  DCR-phase-initial-DET-sale
  DCR-phase-active
  DCR-phase-vote-to-increase-RT
  DCR-phase-vote-to-decrease-RT
  DCR-phase-vote-to-cancel-decrease-RT

  BP-phase-init
  BP-phase-accumulating-stake
  BP-phase-active
  BP-phase-slashed

  BP-slashing-period-days

  credo-phase-no-loan                   ;no link
  credo-phase-loan                      ;gray link
  credo-phase-grace                     ;pink link
  credo-phase-auction                   ;red link
  credo-phase-collective-responsibility ;yellow link???
  credo-phase-repaid                    ;green link

  ; limits
  min-investment-dai
  max-investment-dai
  min-deposit-dai
  max-deposit-dai
;TODO
;  min-loan-dai
;  max-loan-dai
  min-annual-staking-rewards-pct
  max-annual-staking-rewards-pct

  ; DET supply
  DET-circulating-supply  ;DET cicrculating supply
  DET-for-investors       ;DET supply divided between investors

  DET-trading?            ;Did DET token already started trading?
  DET-usd-rate            ;DET current exchange rate
  DET-usd-current-delta   ;DET current delta (used in random walk)
  DET-slippage            ;slippage when trading DET tokens, including commission/fees
  DET-slippage-collective-responsibility

  ; voting-related
  voted-to-lowering-RRT-at-tick ;Voted to decrease RT at tick
  delayed-new-RRT-pct ;Delayed new decreased RT

  sr-tab ; staking rewards table

  investor-strategies ; ["random" "high-risk" "low-risk"]
  investor1 ; a representative investor who's ROI we're measuring
  ;investor1-investment-dai
  ;investor1-strategy
  ;investor1-strategy-split-to-BPs
]

to set-constants
  ; constants

  ; hourly ticks
  ;set TICKS-PER-DAY        1
  ;set TICKS-PER-DAY        24 * 6
  set TICKS-PER-DAY        24
  set TICKS-PER-YEAR       365 * TICKS-PER-DAY
  set BLOCK-TIME-IN-TICKS  TICKS-PER-DAY
  set BLOCKS-PER-YEAR      365 * (TICKS-PER-DAY / BLOCK-TIME-IN-TICKS)

  set investor-strategies ["random" "high-risk" "low-risk"]

  ; DCR colors
  set color-dcr-background      black
  set color-dcr-exsess-reserve  gray
  set color-dcr-deposits        sky
  set color-dcr-capital         violet

  ; voting colors
  set color-investor-non-voting gray
  set color-investor-suspended  yellow
  set color-investor-voted-yes  green
  set color-investor-voted-no   red

  ; DCR visualization bars' width
  set dcr-viz-col1-dx 2
  set dcr-viz-col2-dx 2

  set BPs-hidden?       false
  set investor1-hidden? true  ; init with true to avoid clicking button twice

  ; DCR phases
  set DCR-phase-init                        0
  set DCR-phase-initial-DET-sale            1
  set DCR-phase-active                      2
  set DCR-phase-vote-to-increase-RT         3
  set DCR-phase-vote-to-decrease-RT         4
  set DCR-phase-vote-to-cancel-decrease-RT  5

  ; BP phases
;  set BP-phase-init                         0
;  set BP-phase-accumulating-stake           1
;  set BP-phase-active                       2
;  set BP-phase-slashed                      3
  set BP-phase-init                         "init"
  set BP-phase-accumulating-stake           "accumulating"
  set BP-phase-active                       "active"
  set BP-phase-slashed                      "slashed"

  ; TODO - make a slider or input?
  set BP-slashing-period-days               14

  ;credo phases
;  set credo-phase-no-loan                   0
;  set credo-phase-loan                      1
;  set credo-phase-grace                     2
;  set credo-phase-auction                   3
;  set credo-phase-collective-responsibility 4
;  set credo-phase-repaid                    5
  set credo-phase-no-loan                   "no-loan"
  set credo-phase-loan                      "loan"
  set credo-phase-grace                     "grace"
  set credo-phase-auction                   "auction"
  set credo-phase-collective-responsibility "collective-responsibility"
  set credo-phase-repaid                    "repaid"

  ; limits
  set min-investment-dai       1000    ; 1K DAI
  set max-investment-dai    1000000    ; 1M DAI
  set min-deposit-dai             0.01 ; 0.01 DAI
  set max-deposit-dai    1000000000    ; 100M DAI
;TODO
;  set min-loan-dai              100
;  set max-loan-dai          1000000
  set min-annual-staking-rewards-pct  0.1
  set max-annual-staking-rewards-pct  300

  set DET-slippage                           0.05 ;  5% slippage for single credo liquidation
  set DET-slippage-collective-responsibility 0.15 ; 15% slippage for collective responsibility
end

;"Livepeer" "Loki" "BOScoin" "OKCash" "PIVX" "Decred" "Dash" "Tezos" "Ethereum PoS" "Cardano"

to fill-sr-table
  set sr-tab table:make
  ;                               0       1         2            3              4            5            6              7
  ;                name           symbol  $mktcap   $last-price staking-yield%  min-pos      min-mn       risk           score%
  table:put sr-tab "Livepeer"     ["LPT"   62527107  6.03        155.47         1            40542        "moderate"     88.00 ]
  table:put sr-tab "Loki"         ["LOKI"  7071696   0.184        41.34         7295.56      32774        "moderate"     77.00 ]
  table:put sr-tab "BOScoin"      ["BOS"   11690519  0.0261       32.47         10000        40000        "very risky"   32.47 ] ; "not rated"
  table:put sr-tab "OKCash"       ["OK"     1114665  0.015        28.15         1            1            "very risky"   66.00 ]
  table:put sr-tab "PIVX"         ["PIVX"  47567118  0.803        12.61         1            10000        "stable"       77.00 ]
  table:put sr-tab "Decred"       ["DCR"  167408239  17.95        10.56         122.70568581 122.70568581 "stable"       90.75 ] ; no MN?
  table:put sr-tab "Dash"         ["DASH" 787843451  91.11         8.60         1000         1000         "very stable"  74.25 ]
  table:put sr-tab "Tezos"        ["XTZ"  356360531  0.454         7.73         1            10000        "stable"       82.50 ]
  table:put sr-tab "Ethereum PoS" ["ETH"  17000257450  162.12      4.21         32           32           "moderate"     85.00 ] ; no MN, "not rated"
  table:put sr-tab "Cardano"      ["ADA"  1512872878  0.0485       3.70         10000        10000        "moderate"     85.00 ] ; no MN, "not rated"
end

;"very stable" 0.25
;"stable"      0.50
;"moderate"    2.00
;"risky"       5.00
;"very risky"  8.00
;"not rated"   2.00

; convert risk rating from string to percentage
to-report risk-to-prob-to-be-slashed-pct [risk]
  report
    (cf:ifelse-value
      risk = "very stable" [0.25]
      risk = "stable"      [0.50]
      risk = "moderate"    [2.00]
      risk = "risky"       [5.00]
      risk = "very risky"  [8.00]
      risk = "not rated"   [2.00]
    )
end

to setup-sr-table
  fill-sr-table

  ifelse pos-example = "Use average BP yield minus fees" [
    set min-investor-stake-dai  1
    set min-bp-stake-dai        10000
    set staking-rewards-avg     avg-bp-yield-minus-fees-pct
    set staking-rewards-stddev  precision (staking-rewards-avg * 0.1) 2 ; stddev=10%
  ] [
    let row table:get sr-tab pos-example
    ;print (word "Predefined staking parameters: " row)

    let last-price item 2 row
    let sr         item 3 row
    let min-pos    (item 4 row) * last-price
    let min-mn     (item 5 row) * last-price
    let risk       (item 6 row)
    ;let score      (item 7 row) / 100

    ;TODO
    set min-investor-stake-dai      precision min-pos 2
    set min-bp-stake-dai            precision min-mn 2
    set avg-bp-yield-minus-fees-pct sr
    set staking-rewards-avg         sr
    set staking-rewards-stddev      precision (sr * 0.1) 2 ; stddev=10%

    set annual-prob-to-be-slashed-pct (risk-to-prob-to-be-slashed-pct risk)
  ]
end

to-report DCR-total
  report DCR-capital + DCR-deposits
end

to-report DCR-capital
  report sum [capital-dai] of investors
end

to-report DCR-deposits
  ; TODO: do we take into a/c deposits from BPs as part of DCR?
  ;report (sum [deposit-dai] of depositors)
  ;report (sum [deposit-dai] of depositors) + (sum [deposit-dai] of BPs) + (sum [deposit-dai] of investors)
  report (sum [deposit-dai] of turtles)
end

; Reserve Ratio (RR)
to-report reserve-ratio
  let deposits    DCR-deposits
  let reserves    DCR-capital + deposits
  let liabilities (sum [credos-dai] of credos) + deposits
  if-else liabilities = 0 [
    report 1 ; TODO
  ] [
    report reserves / liabilities
  ]
end



;; ER = Capital + (100% - RR%)/100*Deposits
;; MaxCredos = StakedDETTokens/TotalDETTokens * ER

;; Excess Reserve (L) - maximum total credos that can be given to DET tokens holders
;to-report excess-reserve
;  let reserves (sum [capital-dai + deposit-dai] of investors)
;  let deposits-only (sum [deposit-dai] of investors)
;  let RR reserve-ratio
;  report (reserves - RR * deposits-only) / RR
;end

;; ER = Capital + (100% - RR%)/100*Deposits
to-report excess-reserve
  report (DCR-capital + (100 - RRT-pct) / 100 * DCR-deposits)
end

;; total DET tokens investors have
to-report DET-held-by-investors
  report sum [DET-tokens] of investors
end

to-report DET-staked
  report sum [DET-tokens-staked] of credos
end

;; percentage of DET tokens staked
to-report DET-staked-pct
  ; TODO: QUESTION
  ;report DET-staked / DET-circulating-supply
  report DET-staked / DET-for-investors
end

to-report total-interest-dai
  ;report (sum [credos-dai + interest-dai] of credos) + (sum [deposit-dai] of investors)
  report (sum [interest-dai] of credos) + (sum [deposit-dai] of investors)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  clear-all-plots
  clear-output
  repeat 15 [ print "" ]

  ;seed RNG with time
  random-seed new-seed

  set-constants
  fill-sr-table

  __change-topology false false
  set-patch-size 14
  ;resize-world min-pxcor max-pxcor min-pycor max-pycor
  resize-world -16 (16 + dcr-viz-col1-dx + dcr-viz-col2-dx) -16 16


  set voted-to-lowering-RRT-at-tick 0
  set delayed-new-RRT-pct 0

  set DET-trading? false
  set DET-usd-rate DET-initial-price-dai
  set DET-usd-current-delta 0

  ;set DCR-phase DCR-phase-init
  set DCR-phase DCR-phase-initial-DET-sale
  set RRT-pct initial-RRT-pct

  ;; DET supply
  set DET-circulating-supply DET-initial-supply
  set DET-for-investors DET-circulating-supply * DET-investors-allocation-pct / 100

  ; validations
  ; stddev can't be larger than avg/3  (i.e. six signma)
  validate-staking-rewards-sliders

  ;; set default shapes for breeds
  set-default-shape depositors "person"
  ;set-default-shape investors  "circle"
  ;set-default-shape investors  "person business"
  set-default-shape investors  "person"
  set-default-shape BPs        "box"

  create-depositors num-depositors [
    set hidden?     true
    ;set shape        "person"
    let x (random-float (max-pxcor - min-pxcor - 2 - dcr-viz-col1-dx - dcr-viz-col2-dx)) + min-pxcor
    setxy x random-ycor

    ; 10K+/-1K
    set wallet-dai   (clamp-normal 1 100000 10000 1000)
    set deposit-dai  0
  ]

  create-investors num-investors [
    set hidden? false

    let x (random-float (max-pxcor - min-pxcor - 2 - dcr-viz-col1-dx - dcr-viz-col2-dx)) + min-pxcor
    setxy x random-ycor

    set color color-investor-non-voting
    ;set shape "circle"
    ; TODO
    ;set size (log (wallet-dai / (2 * 3.1415926)) 10) * 0.3
    set size 1.5

    ; 100K+/-10K
    set wallet-dai        (clamp-normal 1000 1000000 100000 10000)
    set deposit-dai       0
    ; TODO - there is no capital-dai - only DET - DET-tokens
    set capital-dai       0
    set DET-tokens        0
    ;set DET-tokens-staked 0

    set suspended?           false
    set voted?               false
    set voted-to-change-RRT? false

    set investor-strategy               one-of investor-strategies
    ;set investor-strategy-split-to-BPs  one-of [1 2 3]
    set investor-strategy-split-to-BPs  (random investor-strategy-split-to-BPs-max) + 1
  ]

  ; remember representative investor who's ROI we're measuring
  set investor1 one-of investors
  ask investor1 [
    set color white
    set size  2

    set wallet-dai                      investor1-investment-dai
    set investor-strategy               investor1-strategy
    set investor-strategy-split-to-BPs  investor1-strategy-split-to-BPs
  ]

  ; set staking reward before creating BPs
  setup-sr-table

  create-BPs num-BPs [
    set hidden?     false
    set color       yellow
    ;set shape        "box"
    let x (random-float (max-pxcor - min-pxcor - 2 - dcr-viz-col1-dx - dcr-viz-col2-dx)) + min-pxcor
    setxy x random-ycor

    ; 20K+/-2K
    set wallet-dai   (clamp-normal 1 200000 20000 2000)
    set deposit-dai  0

    set BP-phase     BP-phase-init
    ;set BP-phase     BP-phase-accumulating-stake

    ; choose random normally distributed  staking rewards %
    let sr (clamp-normal min-annual-staking-rewards-pct max-annual-staking-rewards-pct staking-rewards-avg staking-rewards-stddev)

    set annual-staking-rewards-pct sr
    set label (word (precision sr 1) "%      ")

    ; risk-multiplier =  this-BP-yield / avg-BP-yield
    let risk-multiplier  sr / staking-rewards-avg

    ;adjust min/max p
    let p-slashed-annual-unadjusted-for-risk (clamp-normal 0.001 0.11 (annual-prob-to-be-slashed-pct / 100) (annual-prob-to-be-slashed-pct / 1000))
    ; P(slashing-this-BP) = risk-multiplier * P(slashing avg)
    set p-slashed-annual (clamp 0.0 1.0 (p-slashed-annual-unadjusted-for-risk * risk-multiplier))

    ;;P(S_daily) = 1 - (1 - P(S_annual))^(1/365)
    set p-slashed-daily (1 - (1 - p-slashed-annual) ^ (1 / BLOCKS-PER-YEAR))

    ;print (word "risk-multiplier: " risk-multiplier ", p-slashed-annual-unadjusted-for-risk: " p-slashed-annual-unadjusted-for-risk", p-slashed-annual: " p-slashed-annual )

    ;dailyInterest = (1 + annualInterest)^(1/365) - 1
    ;let sr (1 + annual-staking-rewards-pct / 100) ^ (1 / 365) - 1
     set daily-staking-rewards-multiplier (1 + annual-staking-rewards-pct / 100) ^ (1 / BLOCKS-PER-YEAR)
  ]

  ; layout agents for nicer visualization
  layout-circle investors 13
  layout-circle BPs 8
  ; shift everyone to the left, to not overlap with DCR visualization
  ask turtles with [breed = investors or breed = BPs] [
    set heading -90
    fd 3
  ]

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop of the simulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  if ticks = 0 [
    print "CREDO simulation started"
    print (word "There are " num-investors " investors, " num-BPs " BPs and " num-depositors " depositors")
  ]
                                      ;TODO - need to handle bad credos
;  if (ticks > 3 * TICKS-PER-YEAR) and (not any? credos) [
  if (ticks > 4 * TICKS-PER-YEAR) [
    clear-links
    print "stopping simulation after 3 yrs"
    stop
  ]

  if ticks = 0     [
    canvas-label "Inital RRT set to 100%"
    set RRT-pct 100
    ;put-max-investments-and-deposits
  ]
  if ticks = 2 * TICKS-PER-YEAR     [
    canvas-label "Changing RRT to 90%"
    set RRT-pct 90
  ]
  if ticks = 3 * TICKS-PER-YEAR [
    canvas-label "Changing RRT to 75%"
    set RRT-pct 75
  ]

  if ticks = 25 * TICKS-PER-DAY [
    canvas-label "Investors inject capital into DCR"
  ]
  if (25 * TICKS-PER-DAY <= ticks) and (ticks < 195 * TICKS-PER-DAY) and (ticks mod 10 = 0) [
    ;investors-invest-loop
    investors-invest
  ]
  if ticks = 195 * TICKS-PER-DAY [
    set DET-trading? true
    canvas-label "Depositors make deposits into DCR"
  ]
  if (195 * TICKS-PER-DAY <= ticks) and (ticks < 365 * TICKS-PER-DAY) and (ticks mod 10 = 0) [
    ;depositors-deposit-loop
    depositors-deposit
  ]
  if ticks = TICKS-PER-YEAR [
    ask BPs [
      set BP-phase BP-phase-accumulating-stake
    ]
    canvas-label "Investors take loans and delegate them to BPs"
  ]


  draw-DCR DCR-capital DCR-deposits excess-reserve
  DET-update-price

; TODO - phases

;  ;if ([DET-tokens-staked > 0] of investor1) and (investor1-total <= 0) [
;  if ([any? my-out-credos] of investor1) and (investor1-total <= 0) [
;    ;TODO: print investor1-ROI
;    print (word "Investor1 ROI:" investor1-total)
;    stop
;  ]

  if (ticks > TICKS-PER-YEAR) and (ticks mod BLOCK-TIME-IN-TICKS = 0) [
    check-for-delayed-voting
    BPs-pay-interest
    investors-take-max-loans-prob 10;%
  ]
  check-for-loans-due

  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility reporters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report clamp [min_ max_ x]
  if x < min_ [ report min_ ]
  if x > max_ [ report max_ ]

  report x
end

to-report clamp-normal [min_ max_ avg_ stddev_]
  if stddev_ > (avg_ / 3) [ set stddev_ (avg_ / 3) ]

  let x (random-normal avg_ stddev_)
  if x < min_ [ report min_ ]
  if x > max_ [ report max_ ]

  report x
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent: depositors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; version of depositors-deposit, which runs until no more new deposits can be made
to depositors-deposit-loop
  while [ sum [wallet-dai] of depositors > 0 ] [
    depositors-deposit
  ]
end

to depositors-deposit
  ;pre-conditions
  if deposit-stddev > deposit-avg / 3 [
    ;print (word "*** deposit-stddev > deposit-avg")
    set deposit-stddev precision (deposit-avg / 3) 2
  ]

  if num-depositors + num-BPs = 0 [ print "combined number of depositors and BPs is 0 - please increase!" stop ]
  if deposit-avg = 0              [ print "Average deposit is 0 - please increase!"                       stop ]
  if depositors-deposit-pct = 0   [ print "depositors-deposit-pct = 0 - please increase!"                 stop ]

  ; TODO: deposits are not related to the current DCR phase?

  ;ask turtles with [breed = depositors or breed = BPs or breed = investors] [
  ;ask turtles with [breed = depositors or breed = BPs] [
  ask depositors [
    if (random-float 1.0) < (depositors-deposit-pct / 100) [
      let amt (clamp-normal min-deposit-dai max-deposit-dai deposit-avg deposit-stddev)

      if amt > wallet-dai [
        set amt wallet-dai
      ]
      set wallet-dai  wallet-dai  - amt
      set deposit-dai deposit-dai + amt
    ]
  ]
end

to depositors-withdraw
  ;pre-conditions
  if deposit-stddev > deposit-avg / 3 [
    ;print (word "*** deposit-stddev > deposit-avg")
    set deposit-stddev precision (deposit-avg / 3) 2
  ]

  ; TODO: withdrawals are not related to the current DCR phase?
  ; TODO - this may trigger various events in DCR
  ask depositors [
    if (random-float 1.0) < (depositors-withdraw-pct / 100) [

      ; TODO - maybe determine amount to withdraw based on the amount already deposited, instead of random distribution?
      let amt (clamp-normal min-deposit-dai max-deposit-dai deposit-avg deposit-stddev)

      if amt > deposit-dai [
        set amt deposit-dai
      ]

      set deposit-dai deposit-dai - amt
      set wallet-dai  wallet-dai  + amt
    ]
  ]
  ; TODO - this may trigger various events in DCR
end

to depositors-withdraw-everything
  ; TODO: withdrawals are not related to the current DCR phase?
  ; TODO - this may trigger various events in DCR
  ask depositors [
    ; NOTE: don't switch the order!
    set wallet-dai  wallet-dai + deposit-dai
    set deposit-dai 0
  ]
  ; TODO - this may trigger various events in DCR
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent: investors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make maximum investments and deposits
to put-max-investments-and-deposits
  investors-invest-loop
  depositors-deposit-loop
  set DET-trading? true
end

; version of investors-invest, which runs until no more new investments can be made
to investors-invest-loop
  while [ sum [wallet-dai] of investors > 0 ] [
    investors-invest
  ]
end

to investors-invest
  ;pre-conditions
  if investment-stddev > investment-avg / 3 [
    ;print (word "*** investment-stddev > investment-avg")
    set investment-stddev (precision (investment-avg / 3) 2)
  ]

  ;if DCR-phase = DCR-phase-open-to-investments and DCR-total < hard-cap [
  ask investors [
    ;;TODO
    ;if self = investor1 [
    ;  print who
    ;]

    if (random-float 1.0) < (investors-invest-pct / 100) [
      let amt (clamp-normal min-investment-dai max-investment-dai investment-avg investment-stddev)

      if amt > wallet-dai [
        set amt wallet-dai
      ]

      set wallet-dai  wallet-dai  - amt
      set capital-dai capital-dai + amt

      ;let DET-token-price 1.0 ; TODO
      ;set DET-tokens (investment-amt * DET-token-price)

      ; TODO - respect the cap
      ; TODO - check if this formula is wrong using DET supply graph
      ; TODO - which DET price to use for followup investments?

      let new-DET-tokens (amt / DET-initial-price-dai)
      set DET-tokens (DET-tokens + new-DET-tokens)

      ;set DET-for-investors      (DET-for-investors - new-DET-tokens)
      ;set DET-circulating-supply (DET-circulating-supply + new-DET-tokens)
    ]
  ]
end

;to investors-take-loans
;  ;pre-conditions
;  if loan-stddev > loan-avg / 3 [
;    ;print (word "*** loan-stddev > loan-avg")
;    set loan-stddev (precision (loan-avg / 3) 2)
;  ]
;
;  ;if DCR-phase = DCR-phase-open-to-investments and DCR-total < hard-cap [
;  ask investors [
;    if (random-float 1.0) < (investors-take-loans-pct / 100) [
;      let amt (clamp-normal min-loan-dai max-loan-dai loan-avg loan-stddev)
;
;      if amt > DET-tokens [
;        set amt DET-tokens
;      ]
;      set DET-tokens        (DET-tokens - amt)
;      set DET-tokens-staked (DET-tokens-staked + amt)
;
;      ; TODO - check if there is enough ER
;
;      ;  TODO - convert DET tokens to pro-rata DAI
;      set credos-dai        amt
;
;      ; delegate to BP
;      ; TODO ...
;    ]
;  ]
;end

to investors-take-max-loans
  investors-take-max-loans-prob investors-take-loans-pct
end

to investors-take-max-loans-prob [prob-take-loan-pct]
  ;if DCR-phase = DCR-phase-open-to-investments and DCR-total < hard-cap [
  ask investors [
    if ((random-float 1.0) < (prob-take-loan-pct / 100)) [

      ; if we still can take credos - i.e. any DET left
      if (DET-tokens > 0) [
        ; how much credos can we take assuming we stake all our unstaked/unlocked DET tokens
        let credos-dai_  excess-reserve * (DET-tokens / DET-circulating-supply)

        let delegated-to-BPs  []
        let non-slashed-BPs   BPs with [BP-phase = BP-phase-active or BP-phase = BP-phase-accumulating-stake]
        let nBPs              min (list investor-strategy-split-to-BPs (count non-slashed-BPs))

        ; TODO take into a/c minimum stake for investor
        while [(nBPs > 0) and ((credos-dai_ / nBPs) < min-investor-stake-dai)] [
          print (word "nBPs=" nBPs ", credos-dai_=" credos-dai_ ", credos-dai_/nBPs=" (credos-dai_ / nBPs))
          set nBPs nBPs - 1
        ]
        ; min investor stake is too high!
        if nBPs <= 0 [
          print (word "min-investor-stake-dai (" min-investor-stake-dai ") is too high!")
          stop
        ]

        if investor-strategy = "random" [
          ; delegate to a random BP
          set delegated-to-BPs   n-of nBPs non-slashed-BPs
        ]
        if investor-strategy = "high-risk" [
          ; delegate to the top paying BP (heighest percieved probability to be slashed)
          set delegated-to-BPs   max-n-of nBPs non-slashed-BPs [annual-staking-rewards-pct] ; TODO - MINUS  management-fee-pct
        ]
        if investor-strategy = "low-risk" [
          ; delegate to the most safe BP (least percieved probability to be slashed)
          ;set delegated-to-BPs   min-n-of nBPs non-slashed-BPs [p-slashed-daily]
          set delegated-to-BPs   min-n-of nBPs non-slashed-BPs [annual-staking-rewards-pct] ; TODO - MINUS  management-fee-pct
        ]

        let nBPs-actual (count delegated-to-BPs)
        let DET-tokens-before-staking DET-tokens
        set DET-tokens 0

        ask delegated-to-BPs [
          ;create directed link of breed credos from this investor to chosen delegated BP
          ;here myself is investor and self is BP
          ;create credo from myself/Investor to self/BP
          let thickness_ ifelse-value (myself = investor1) [0.2] [0.1]
          create-credo-from myself [
            set thickness      thickness_
            ; TODO - check if there is enough ER
            ; TODO - convert DET tokens to pro-rata DAI
            set credo-phase       credo-phase-loan
            set credos-dai        (credos-dai_ / nBPs-actual)
            set DET-tokens-staked (DET-tokens-before-staking / nBPs-actual)
            set credo-taken-at    ticks
            set credo-due-at      ticks + max-loan-period-days * TICKS-PER-DAY
            set interest-dai      0
          ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; credos - find loans due pay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to check-for-loans-due
  ask credos [
    if (credo-phase = credo-phase-repaid) and (ticks >= delete-after-repayment-at) [
      ;TODO
      ; return money to investor
      let DET-tokens-staked2 DET-tokens-staked
      let interest-dai2 interest-dai
      ask end1 [
        ;print (word "*** repaid " DET-tokens-staked2 " " interest-dai2)
        set DET-tokens  DET-tokens + DET-tokens-staked2
                        ;TODO
        set deposit-dai deposit-dai + interest-dai2
      ]
      set DET-tokens-staked 0
      set interest-dai 0
      die
    ]

    if (credo-phase = credo-phase-loan) and (ticks >= credo-due-at) [

      if-else (random-float 1.0) < (prob-loan-repayment-pct / 100) [
        set credo-phase credo-phase-repaid
        set color credo-phase-to-color credo-phase
        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY
      ] [
        set credo-phase credo-phase-grace
        set color credo-phase-to-color credo-phase
        set grace-period-due ticks + grace-period-days * TICKS-PER-DAY
      ]
    ]

    if (credo-phase = credo-phase-grace) and (ticks >= grace-period-due) [
      if-else (random-float 1.0) < (prob-repayment-grace-pct / 100) [
        set credo-phase credo-phase-repaid
        set color credo-phase-to-color credo-phase
        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY
      ] [
        set credo-phase credo-phase-auction
        set color credo-phase-to-color credo-phase
      ]
    ]

    if credo-phase = credo-phase-auction [
      let DET-usd-rate-adjusted (DET-usd-rate * (1 + DET-slippage))
      let max-collateral-dai DET-tokens-staked * DET-usd-rate-adjusted
      if-else max-collateral-dai > credos-dai [
        ; sell DET tokens collateral
        set DET-tokens-staked DET-tokens-staked - credos-dai / DET-usd-rate-adjusted
        set credos-dai 0
        print (word "Credo " self ": collateral " (precision DET-tokens-staked 2) " DET tokens were auctioned - loan fully repaid")

        ; switch credo to repaid phase
        set credo-phase credo-phase-repaid
        set color credo-phase-to-color credo-phase
        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY
      ] [
        ; sell all collateral DET tokens and recover part of the credos loan
        print (word "Credo " self ": collateral " (precision DET-tokens-staked 2) " DET tokens were auctioned - loan partially repaid")
        set credos-dai credos-dai - max-collateral-dai
        set DET-tokens-staked 0

; TODO - TEMPORARY - use repaid instead of collective responsibility
        ; switch credo to repaid phase
        set credo-phase credo-phase-repaid
        set color credo-phase-to-color credo-phase
        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY

;        set credo-phase credo-phase-collective-responsibility
;        set color credo-phase-to-color credo-phase
;        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY
      ]
    ]

    if credo-phase = credo-phase-collective-responsibility [
      ; split the remaining credos-dai between all DET holders
      let DET-usd-rate-adjusted (DET-usd-rate * (1 + DET-slippage-collective-responsibility))
      let DET-tokens-to-sell (credos-dai / DET-usd-rate-adjusted)
      let num-investors-with-DET (count other investors)
      let total-DET-tokens-staked (sum [sum [DET-tokens-staked] of my-out-credos] of other investors)
      if-else (num-investors-with-DET > 0) and (total-DET-tokens-staked * DET-usd-rate-adjusted <= credos-dai) [
        let DET-to-forfeit-from-each-investor (DET-tokens-to-sell / num-investors-with-DET)
        print (word "*** Collective Responsibility phase - forefeiting " DET-to-forfeit-from-each-investor " staked DET from each investor. Total " DET-tokens-to-sell " DET")

        ask other investors [
          let num-credos (count my-out-credos with [DET-tokens-staked > 0])
          if num-credos > 0 [
            let DET-to-forfeit-per-credo (DET-to-forfeit-from-each-investor / num-credos)
            ask my-out-credos with [DET-tokens-staked > 0] [
              set DET-tokens-staked (DET-tokens-staked - DET-to-forfeit-per-credo)
              if DET-tokens-staked < 0 [
                set DET-tokens-staked 0
              ]
            ]
          ]
        ]

        set credo-phase credo-phase-repaid
        set color credo-phase-to-color credo-phase
        set delete-after-repayment-at ticks + 14 * TICKS-PER-DAY
      ] [
        ;TODO - NOT IMPLEMENTED
        ;TODO - liquidation?
        print "*** DCR Liquidation phase"
      ]
    ]
  ]
end

;; convert credo phase to color
to-report credo-phase-to-color [phase]
  report
    (cf:ifelse-value
      phase = credo-phase-repaid                    [green ]
      phase = credo-phase-no-loan                   [black ]
      phase = credo-phase-loan                      [gray  ]
      phase = credo-phase-grace                     [yellow]
      phase = credo-phase-auction                   [red   ]
      phase = credo-phase-collective-responsibility [orange]
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BPs - Block Producers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; toggle show/hide BPs and credos links from Investors to BPs
to show-hide-BPs
  set BPs-hidden? (not BPs-hidden?)
  ask BPs [
    set hidden? BPs-hidden?
    ask my-in-credos [
      set hidden? BPs-hidden?
    ]
  ]
  ;ask credos [
  ;  set hidden? BPs-hidden?
  ;]
end

to show-hide-investor1
  set investor1-hidden? (not investor1-hidden?)

  ifelse investor1-hidden? [
    ; show all investors1, BPs and credos
    ask investors [set hidden? false]
    ask BPs       [set hidden? false]
    ask credos    [set hidden? false]
  ] [
    ; first hide all investors1, BPs and credos
    ask investors [set hidden? true]
    ask BPs       [set hidden? true]
    ask credos    [set hidden? true]

    ; then show only BPs and credos for investor1
    ask investor1 [
      set hidden? false
      ;all BPs this investor delegated credos to
      ask out-credo-neighbors [set hidden? false]
      ;all credos taken and delegated by this investor to BPs
      ask my-out-credos [set hidden? false]
    ]
  ]
end

to-report investor1-total
  let total-dai 0
  ask investor1 [
    ;all credos taken and delegated by this investor to BPs
    set total-dai deposit-dai + (sum [interest-dai] of my-out-credos)
  ]
  report total-dai
end

to-report investor1-ROI
  let ROI [
    ifelse-value (capital-dai = 0)
      [0]
      [(deposit-dai + (sum [interest-dai] of my-out-credos)) / capital-dai]
  ] of investor1

  report precision (ROI * 100) 2
end

to-report investors-aggregated-ROI
  let total-capital  sum [capital-dai] of investors
  let total-interest (sum [deposit-dai] of investors) + (sum [interest-dai] of credos)

  let ROI ifelse-value (total-capital = 0) [0] [total-interest / total-capital]

  report precision (ROI * 100) 2
end


;to test1
; ask credos []                ;all credos
;
; ask investors [
;   ask out-credo-neighbors [] ;all BPs this investor delegated credos to
;   ask my-out-credos []       ;all credos taken and delegated by this investor to BPs
; ]
;
; ask BPs [
;   ask in-credo-neighbors [] ;all investors delegated credo to this BP
;   ask my-in-credos []       ;all credos links delegated to this BP
; ]
;end

to validate-staking-rewards-sliders
  ; stddev can't be larger than third of the mean (i.e. 6 sigma)
  if staking-rewards-stddev > staking-rewards-avg / 3 [
    set staking-rewards-stddev (staking-rewards-avg / 3)
  ]
end

; TODO: interest -> staking rewards
to BPs-pay-interest
  ;validate-staking-rewards-sliders

  ask BPs [
    let stake-dai BPs-stake

    ; went below min stake requirement - switch to ACCUMULATING phase
    (cf:ifelse
      ((BP-phase = BP-phase-accumulating-stake) and (stake-dai < min-bp-stake-dai)) [
        set shape "box"
        set color gray
      ]
      ((BP-phase = BP-phase-accumulating-stake) and (stake-dai >= min-bp-stake-dai)) [
        set BP-phase BP-phase-active
        set shape "box"
        set color yellow
        print (word "BP " who " accumulated stake of " (precision stake-dai 0) " DAI. Switching to ACTIVE - started reward payouts")
      ]
      ; check if need to be unslashed
      ((BP-phase = BP-phase-slashed) and (ticks > unslash-at) and (stake-dai < min-bp-stake-dai)) [
        set shape "fire"
        set color red

        set BP-phase BP-phase-accumulating-stake
        set slashed-at 0
        set unslash-at 0
      ]
      ((BP-phase = BP-phase-slashed) and (ticks > unslash-at) and (stake-dai >= min-bp-stake-dai)) [
        set shape "box"
        set color yellow

        set BP-phase BP-phase-active
        set slashed-at 0
        set unslash-at 0
      ]
; TODO - BP oscilating bug (maybe not a bug)
      ((BP-phase = BP-phase-active) and (stake-dai < min-bp-stake-dai)) [
        set BP-phase BP-phase-accumulating-stake
        set shape "box"
        set color gray
        print (word "BP " who " stake " (precision stake-dai 0) " DAI went below min requirement. Switched to ACCUMULATING - no reward payouts")
      ]
      ; if this BP is active, let's check if it's randomly slashed
      (BP-phase = BP-phase-active) [
        BP-active
      ]
    )
  ]
end

to BP-active
    ; if this BP been slashed
    if-else (random-float 1.0) < p-slashed-daily [
      set shape "fire"
      set color red

      set BP-phase BP-phase-slashed
      set slashed-at ticks
      set unslash-at ticks + BP-slashing-period-days * TICKS-PER-DAY

      let total-slashed deposit-dai
      set deposit-dai 0

      ;        let total-stake 0
      ;        if-else staking-compounding? [
      ;          set total-stake sum [credos-dai + interest-dai] of credos
      ;        ] [
      ;          set total-stake sum [credos-dai] of credos
      ;        ]

      ;; ask all incoming credos links to this BP
      ask my-in-credos [
        set color (credo-phase-to-color credo-phase)
        if-else staking-compounding? [
          set total-slashed total-slashed + (credos-dai + interest-dai)
          set credos-dai   0
          set interest-dai 0
        ] [
          set total-slashed total-slashed + credos-dai
          set credos-dai   0
        ]
        ;die ; kill this credo link
      ]
      ask my-in-credos [
        ;if credo-phase = credo-phase-collective-responsibility [
        ;if credo-phase = credo-phase-repaid or credo-phase = credo-phase-auction [
        if credo-phase = credo-phase-repaid [
          ;TODO
          die
        ]
      ]

      print (word "BP " who " is slashed. Total stake lost: " (precision total-slashed 0) " DAI, # credo loans recalled: " count my-in-credos)

    ] [
      ; does this BP has a minimum stake?
      ; TODO - check that not slashed
      let all-delegated-credos-dai   sum [credos-dai + interest-dai] of my-in-credos

      ifelse (deposit-dai + all-delegated-credos-dai) < min-bp-stake-dai [
        set BP-phase BP-phase-accumulating-stake
        ;this BP has no minimum stake
        set shape "box"
        set color gray
      ] [
        set BP-phase BP-phase-active
        set shape "box"
        set color yellow

        let dsrm daily-staking-rewards-multiplier
        ;TODO
        ;set deposit-dai deposit-dai * dsrm

        ;; ask all incoming credos links to this BP
        ask my-in-credos [
          if-else staking-compounding? [
            ;compounding interest
            set interest-dai  interest-dai + (credos-dai + interest-dai) * (dsrm - 1)
          ] [
            ;non-compounding interest
            set interest-dai  interest-dai + credos-dai * (dsrm - 1)
          ]
        ]
      ]
    ]
end

;calculate stake of this BP
to-report BPs-stake
  let all-credos-dai 0

  ;; my-in-credos - all incoming credos links to this BP
  if-else staking-compounding? [
    set all-credos-dai (sum [credos-dai + interest-dai] of my-in-credos)
  ] [
    set all-credos-dai (sum [interest-dai] of my-in-credos)
  ]

  report deposit-dai + all-credos-dai
end

; TODO - calculate total stake of BP
to-report total-BPs-stake
  let all-credos-dai 0

  ;TODO
  ;ask BPs with [BP-phase != BP-phase-slashed] [
  ;ask BPs with [BP-phase = BP-phase-active or BP-phase = BP-phase-accumulating-stake] [
  ask BPs [
    ;; my-in-credos - all incoming credos links to this BP
    if-else staking-compounding? [
      set all-credos-dai (sum [credos-dai + interest-dai] of my-in-credos)
    ] [
      set all-credos-dai (sum [interest-dai] of my-in-credos)
    ]
  ]

  report deposit-dai + all-credos-dai
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; canvas-label "Investors injected capital into DCR"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to canvas-label [s]
  ;ask patch 0 15 [set plabel s]
  ask patch 0 16 [set plabel s]
  print s
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DCR visualization with 2 bars/columns
;;
;;  +----------------+----------------+
;;  | Deposits       |                |
;;  +----------------+----------------+
;;  | Capital        | Excess Reserve |
;;  +----------------+----------------+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to draw-DCR [capital_ deposits_ excess-reserve_]

  ; TODO
  let total (capital_ + deposits_)
  let my 1
  if total > 0 [
    set my (max-pycor - min-pycor + 1) / total
  ]

  let y0 min-pycor
  let y1 (y0 + my * capital_)
  let y2 (y1 + my * deposits_)

  let x1 (max-pxcor - dcr-viz-col1-dx - dcr-viz-col2-dx - 1)
  let x2 (max-pxcor - dcr-viz-col2-dx)

;  if ticks mod 10000 = 0 [
;    print x1
;    print x2
;    print max-pxcor
;  ]

  ;; DCR visualization - first column - bottoms up
  ask patches with [ x1 <= pxcor and pxcor < x2 and y0 <= pycor and pycor < y1 ] [ set plabel ""  set pcolor color-dcr-capital  ]
  ask patches with [ x1 <= pxcor and pxcor < x2 and y1 <= pycor and pycor < y2 ] [ set plabel ""  set pcolor color-dcr-deposits ]

  ask patches with [ pxcor = x1 + 1 and pycor = round(y0) and pcolor = color-dcr-capital  ] [ set plabel "CAPITAL"  ]
  ask patches with [ pxcor = x1 + 1 and pycor = round(y1 + 1) and pcolor = color-dcr-deposits ] [ set plabel "DEPOSITS" ]

  if-else excess-reserve_ != 0 [
    let y3 (y0 + my * excess-reserve_)
    ;; DCR visualization - second column - bottoms up
    ask patches with [ x2 <= pxcor and y0 <= pycor and pycor <  y3 ] [ set plabel ""  set pcolor color-dcr-exsess-reserve ]
    ask patches with [ x2 <= pxcor and y0 <= pycor and pycor >= y3 ] [ set plabel ""  set pcolor color-dcr-background     ]

    ask patches with [ pxcor = x2 + 1 and pycor = round(y0) and pcolor = color-dcr-exsess-reserve ] [ set plabel "ER"  ]
  ] [
    ask patches with [ x2 <= pxcor and y0 <= pycor ] [ set plabel ""  set pcolor color-dcr-background ]
  ]
end

;;----------------------------------------------------
;; toggle DET token price trading on/off
;;----------------------------------------------------

to flip-DET-trading
  set DET-trading? not DET-trading?
end

;;----------------------------------------------------
;; random walk for the token price
;;----------------------------------------------------
to DET-update-price
  if DET-trading? [
    let DET-usd-price-floor 0.01

    if (random-float 1.0) < 0.1 [
      set DET-usd-current-delta (0.5 - (random-float 1.0)) * 0.01 + market-dir-bias * 0.0001
    ]
    set DET-usd-rate (DET-usd-rate + DET-usd-current-delta)
    if DET-usd-rate < DET-usd-price-floor [
      set DET-usd-rate DET-usd-price-floor
      set DET-usd-current-delta (- DET-usd-current-delta)
    ]
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VOTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A simple majority (i.e. 50%) of voting power is required to increase the reserve target (RT%),
;; while a certain predefined supermajority (e.g. 70% of voting power) is required to decrease it

;; Passive DET tokens Holders are those who hold unstaked DET tokenss.
;; They’re denied voting power, in order to minimize the influence of speculators
;; who are not necessarily aligned with the DAO’s long term interests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report total-voting-power
  report sum [DET-tokens] of investors with [not suspended? and voted?]
end

to-report total-voting-power-yes
  report sum [DET-tokens] of investors with [not suspended? and voted? and voted-to-change-RRT?]
end

to-report total-voting-power-no
  report sum [DET-tokens] of investors with [not suspended? and voted? and not voted-to-change-RRT?]
end

to-report spuermajority-voting-power
  report (sum [DET-tokens] of investors with [not suspended? and voted?]) * supermajority-pct / 100
end

to check-for-delayed-voting
  ;TODO: or check DCR state
  if voted-to-lowering-RRT-at-tick != 0 and
     ticks > voted-to-lowering-RRT-at-tick + delay-for-lowering-RRT-days * TICKS-PER-DAY [

    ; TODO - it doesn't go into effect immediately
    ; only after  delay-for-lowering-rt-days
    ; if not canceled by 50% during that time
    ; TODO: need to notify depositor agents - when RT lowered

    set RRT-pct delayed-new-RRT-pct
    ;TODO: or change DCR state
    set voted-to-lowering-RRT-at-tick 0
    print (word "Delay period of " delay-for-lowering-RRT-days " days has ended. Supermajority of voting power voted FOR decreasing RRT to " RRT-pct "%")
  ]
end

to investors-vote
  ; TODO - use DCR voting FSM ?
  if-else voted-to-lowering-RRT-at-tick != 0 [
    print "Voting period in progress. Can't vote now."
  ] [
    if-else new-RRT-pct = RRT-pct [
      print "no change in RRT - no voting"
    ] [
      investors-vote-round

      let yes-s total-voting-power-yes
      let no-s  total-voting-power-no
      let pct 0
      if-else (yes-s + no-s) = 0 [
        ; TODO - probably related to voted? setting to false
        print "WARNING: (yes-s + no-s) = 0 ln.531"
        set pct 100
      ] [
        set pct yes-s / (yes-s + no-s) * 100
      ]

      if-else new-RRT-pct > RRT-pct [
        ; 50% required
        if-else pct >= 50 [
          set RRT-pct new-RRT-pct
          print (word "majority of voting power voted FOR increasing RRT to " new-RRT-pct "%")
        ] [
          print (word "majority of voting power voted AGAINST increasing RRT to " new-RRT-pct "%")
        ]
      ] [
        ; supermajoirty required
        if-else pct >= supermajority-pct [
          set voted-to-lowering-RRT-at-tick ticks

          ; TODO - it doesn't go into effect immediately
          ; only after  delay-for-lowering-RRT-days
          ; if not canceled by 50% during that time
          ; TODO: need to notify depositor agents - when RRT lowered
          set delayed-new-RRT-pct new-RRT-pct
          ;set RRT-pct new-RRT-pct
          print (word "supermajority of voting power voted FOR decreasing RRT to " new-RRT-pct "%. Change delayed by " delay-for-lowering-RRT-days " days")
        ] [
          print (word "supermajority of voting power voted AGAINST decreasing RRT to " new-RRT-pct "%")
        ]
      ]
    ]
  ]
end

to investors-vote-round
  ;ask investors with [ not suspended? ] [
  ask investors [
    if-else suspended? [
      set color color-investor-suspended
    ] [
      set color color-investor-non-voting
      if (random-float 1.0) < (investors-active-pct / 100) [
        if-else (random-float 1.0) < (investors-voted-for-pct / 100) [
          set color color-investor-voted-yes
          set voted? true
          set voted-to-change-RRT? true
        ] [
          set color color-investor-voted-no
          set voted? true
          set voted-to-change-RRT? false
        ]
      ]
    ]
  ]
end

to investors-vote-to-cancel-decreasing-rt
  ; TODO - check DCR state
  if-else voted-to-lowering-RRT-at-tick = 0 [
    print "No delayed votes to cancel"
  ] [
    investors-vote-round

    let yes-s total-voting-power-yes
    let no-s  total-voting-power-no
    let pct 0
    if-else (yes-s + no-s) = 0 [
      ; TODO - probably related to voted? setting to false
      print "WARNING: (yes-s + no-s) = 0  ln.599"
      set pct 100
    ] [
      set pct yes-s / (yes-s + no-s) * 100
    ]

    if-else pct >= 50 [
      print (word "majority of voting power voted FOR canceling decreasing RRT to " delayed-new-RRT-pct "%")
      set voted-to-lowering-RRT-at-tick 0
      set delayed-new-RRT-pct 0
    ] [
      print (word "majority of voting power voted AGAINST canceling decreasing RRT to " delayed-new-RRT-pct "%")
    ]
  ]
end

to clear-votes
  ask investors [
    if-else suspended? [
      set color color-investor-suspended
    ] [
      set color color-investor-non-voting
    ]
    set voted? false
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
330
30
856
501
-1
-1
14.0
1
10
1
1
1
0
0
0
1
-16
20
-16
16
0
0
1
ticks
30.0

BUTTON
35
650
109
684
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
117
650
181
684
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
27
240
260
273
num-investors
num-investors
0
1000
9.0
1
1
NIL
HORIZONTAL

TEXTBOX
22
10
224
40
CREDO simulation - 04/03/2019
12
0.0
1

PLOT
913
28
1202
203
DCR
time
DAI
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"ER" 1.0 0 -7500403 true "" "plot excess-reserve"
"deposits" 1.0 0 -13791810 true "" "plot DCR-deposits"
"capital" 1.0 0 -8630108 true "" "plot DCR-capital"

TEXTBOX
821
7
862
25
DCR
12
0.0
1

SLIDER
37
715
282
748
staking-rewards-avg
staking-rewards-avg
0
300
20.0
1
1
%
HORIZONTAL

PLOT
1435
30
1635
200
Reserve Ratio (RR)
time
RR
0.0
10.0
-1.5
1.5
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot reserve-ratio"

SLIDER
28
274
258
307
num-depositors
num-depositors
0
1000
200.0
1
1
NIL
HORIZONTAL

BUTTON
333
652
573
686
investors invest
ask investors [ investors-invest ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
591
652
793
686
NIL
depositors-deposit
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
810
770
1058
804
investors take max loans
investors-take-max-loans\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1435
585
1668
618
investors-voted-for-pct
investors-voted-for-pct
0
100
16.0
1
1
%
HORIZONTAL

BUTTON
1433
733
1567
767
NIL
investors-vote
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1436
548
1669
581
new-RRT-pct
new-RRT-pct
0
100
15.0
1
1
%
HORIZONTAL

TEXTBOX
332
8
647
34
Investors and BPs - investor1 is highlighted\n
12
0.0
1

SLIDER
1435
621
1670
654
investors-active-pct
investors-active-pct
0
100
16.0
1
1
%
HORIZONTAL

BUTTON
591
732
791
766
NIL
depositors-withdraw
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
591
692
792
725
depositors-withdraw-pct
depositors-withdraw-pct
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
591
546
793
579
depositors-deposit-pct
depositors-deposit-pct
0
100
1.0
1
1
%
HORIZONTAL

PLOT
1435
360
1635
510
voting power
time
votes
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"pro" 1.0 0 -13840069 true "" "plot total-voting-power-yes"
"cons" 1.0 0 -2674135 true "" "plot total-voting-power-no"
"SupMj" 1.0 0 -7500403 true "" "plot spuermajority-voting-power"

SLIDER
591
582
792
615
deposit-avg
deposit-avg
0
100000
5000.0
1000
1
DAI
HORIZONTAL

SLIDER
592
618
795
652
deposit-stddev
deposit-stddev
0
10000
1000.0
1
1
DAI
HORIZONTAL

SLIDER
868
28
901
504
RRT-pct
RRT-pct
0
100
75.0
1
1
%
VERTICAL

PLOT
1209
27
1415
203
RRT%
time
%
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot RRT-pct"

PLOT
961
361
1163
511
DET/USD price
time
USD
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot DET-usd-rate"

SLIDER
22
60
268
93
DET-initial-supply
DET-initial-supply
100000
10000000
100000.0
100000
1
DET
HORIZONTAL

SLIDER
25
175
273
208
DET-investors-allocation-pct
DET-investors-allocation-pct
0
100
14.0
1
1
%
HORIZONTAL

PLOT
1168
361
1368
511
% DET staked
time
%
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot DET-staked-pct"

PLOT
1435
205
1635
355
Investor's DET
time
DET
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"circ" 1.0 0 -16777216 true "" "plot DET-circulating-supply"
"invst" 1.0 0 -13791810 true "" "plot DET-held-by-investors"

BUTTON
1572
732
1670
766
NIL
clear-votes\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
22
97
270
130
initial-investment-cap
initial-investment-cap
100000
10000000
100000.0
100000
1
USD
HORIZONTAL

TEXTBOX
40
693
148
711
Block Producers
12
0.0
1

SLIDER
25
136
271
169
DET-initial-price-dai
DET-initial-price-dai
0
100
10.0
0.01
1
DAI
HORIZONTAL

SLIDER
918
364
951
512
market-dir-bias
market-dir-bias
-2
2
1.0
1
1
NIL
VERTICAL

BUTTON
2635
739
2777
772
flip DET trading
flip-DET-trading
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1436
657
1668
690
supermajority-pct
supermajority-pct
50
100
0.0
1
1
%
HORIZONTAL

BUTTON
181
650
269
684
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
811
545
1059
578
max-loan-period-days
max-loan-period-days
0
365
90.0
1
1
days
HORIZONTAL

SLIDER
812
582
1059
615
grace-period-days
grace-period-days
0
31
5.0
1
1
days
HORIZONTAL

TEXTBOX
36
36
209
64
#1 - DET Initial Sale
12
0.0
1

SLIDER
1433
693
1669
726
delay-for-lowering-rrt-days
delay-for-lowering-rrt-days
0
7
1.0
1
1
days
HORIZONTAL

BUTTON
1434
772
1666
805
vote-to-cancel-decreasing-rt
investors-vote-to-cancel-decreasing-rt\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
29
310
261
343
num-BPs
num-BPs
0
100
6.0
1
1
NIL
HORIZONTAL

BUTTON
591
772
791
806
withdraw-everything
depositors-withdraw-everything
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
334
547
571
580
investors-invest-pct
investors-invest-pct
0
100
1.0
1
1
%
HORIZONTAL

SLIDER
331
582
570
615
investment-avg
investment-avg
10000
1000000
100000.0
10000
1
DAI
HORIZONTAL

SLIDER
333
617
573
650
investment-stddev
investment-stddev
0
1000000
10000.0
1
1
DAI
HORIZONTAL

TEXTBOX
30
219
237
238
Number of agents of each type\n
12
0.0
0

TEXTBOX
337
521
502
551
DCR Capital injection
12
0.0
1

TEXTBOX
596
520
783
548
Deposits & Withdrawals\n
12
0.0
1

TEXTBOX
1450
523
1566
542
Voting
12
0.0
1

TEXTBOX
816
519
932
537
Loans
12
0.0
1

SLIDER
811
616
1059
649
investors-take-loans-pct
investors-take-loans-pct
0
100
1.0
1
1
%
HORIZONTAL

BUTTON
37
910
185
944
NIL
BPs-pay-interest\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
32
564
302
597
annual-prob-to-be-slashed-pct
annual-prob-to-be-slashed-pct
0
10
3.0
0.5
1
%
HORIZONTAL

SLIDER
37
750
282
783
staking-rewards-stddev
staking-rewards-stddev
0
100
2.0
1.00
1
%
HORIZONTAL

PLOT
960
210
1164
360
Aggregated ROI (%)
time
%
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot investors-aggregated-ROI"

SWITCH
36
872
259
905
staking-compounding?
staking-compounding?
0
1
-1000

BUTTON
191
910
304
945
NIL
show-hide-BPs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
37
787
281
820
min-bp-stake-dai
min-bp-stake-dai
0
300000
10000.0
100
1
DAI
HORIZONTAL

BUTTON
437
812
714
846
NIL
put-max-investments-and-deposits
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
32
481
299
526
PoS-example
PoS-example
"Use average BP yield minus fees" "Livepeer" "Loki" "BOScoin" "OKCash" "PIVX" "Decred" "Dash" "Tezos" "Ethereum PoS" "Cardano"
0

SLIDER
37
823
297
856
min-investor-stake-dai
min-investor-stake-dai
0.01
1000000
1.0
1
1
DAI
HORIZONTAL

SLIDER
32
608
264
641
initial-RRT-pct
initial-RRT-pct
0
100
100.0
1
1
%
HORIZONTAL

PLOT
1167
208
1367
358
ROI - Investor #1 (%)
time
%
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot investor1-ROI"

SLIDER
32
530
301
563
avg-bp-yield-minus-fees-pct
avg-bp-yield-minus-fees-pct
0
200
20.0
1
1
%
HORIZONTAL

CHOOSER
30
390
290
435
investor1-strategy
investor1-strategy
"random" "high-risk" "low-risk"
0

SLIDER
30
355
290
388
investor1-investment-dai
investor1-investment-dai
0
1000000
100000.0
10000
1
DAI
HORIZONTAL

BUTTON
2630
801
2786
835
NIL
inspect investor1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
30
435
282
468
investor1-strategy-split-to-BPs
investor1-strategy-split-to-BPs
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
811
652
1061
685
investor-strategy-split-to-BPs-max
investor-strategy-split-to-BPs-max
1
5
3.0
1
1
NIL
HORIZONTAL

BUTTON
2630
841
2805
875
NIL
show-hide-investor1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
810
693
1062
726
prob-loan-repayment-pct
prob-loan-repayment-pct
0
100
90.0
1
1
%
HORIZONTAL

SLIDER
810
730
1060
763
prob-repayment-grace-pct
prob-repayment-grace-pct
0
100
80.0
1
1
%
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

CREDO DCR (Decentralized Credit Reserve) simulation.

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
