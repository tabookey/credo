### CREDO: decentralized "equity"

Decentralized "equity" solves for global decentralized networks the problem that equity solves for legal corporations, minus the centralization around a legal entity. 

It can be used to reward contributions of all kinds. The trick is a limited form of credit expansion within the micro-economy of a project using CREDO.

This repository contains an agent-model that demonstrates how CREDO would work for a DPoS layer 2 chain. 

CREDO is designed to be incentive compatible and have a path to regulatory compliance by passing the Howey test (not benefiting passively from the work of others) and Hinman test (bootstrappers can walk away).

Explanatory materials:

1) Best place to start is with the [CoRe (Contributor Reward) staking library](https://github.com/tabookey/CoRe-staking/blob/master/README.md), which is a simplified MVP implementation of CREDO's crypto economics that demonstrates the key principles.

2) Watch a video of the high-level [EthCC talk](https://www.youtube.com/watch?v=l17NjaRV4yk).

3) Read the full [CREDO paper](https://docs.google.com/document/d/18pApYxgNXnKh4gXyendGCTyllDkoijmjzkM1CD86bog/edit?ts=5c6bd8f1).

Legal analysis:

1) [Legal memo](https://docs.google.com/document/d/1ZwsWfrXAUzamzZ3PqKcGCrRHHsaWENzDbqic3OM-iGU) by Ziv Keinan
2) [Token Taxonomy Frameworks](https://medium.com/cryptolawreview/token-taxonomy-frameworks-de968bf2605c) on Crypto Law Review, includes a CREDO case study.

Technical materials:

This repo contains a NetLogo source code with an agent-based model for CREDO. The video below shows a single run of a simulation (3 years in simulated time):

[![CREDO Simulation](http://img.youtube.com/vi/4qOFRkm5gtA/0.jpg)](https://www.youtube.com/watch?v=4qOFRkm5gtA "CREDO Simulation")

How to run
==========

1. Download and install NetLogo desktop application (not NetLogo Web) on your machine from this URL:
https://ccl.northwestern.edu/netlogo/download.shtml

2. Launch the NetLogo desktop application and maximize its main window.
3. Save the [credo.nlogo](https://raw.githubusercontent.com/nivertech/credo-abm-sim/master/credo.nlogo?token=AAEynzPLKdbJoPJxEsWC9tY3YlRbnWwfks5cmTA3wA%3D%3D) file (when saving change its file extension to `.nlogo`) and open it in the NetLogo application.
4. Zoom-in / zoom-out the UI to you liking using `Ctrl-"+"` / `Ctrl-"-"` (`⌘Cmd-"+"` / `⌘Cmd-"-"` on Mac).
5. The model's input parameters are in the left upper corner above the `Setup`, `Go` and `Go Once` buttons.
6. Change the input parameters to your liking or leave them as default for the first time.
7. Click on `Setup` and then wait for setup to finish.
8. Click on `Go` - the simulation will start running for 3 years of simulated time or until you'll unclick the `Go` button (the simulation runs while the `Go` button is pressed).
9. The results of the simulation can be seen on the plots in the right part of the screen, i.e. specifically `Investor1` (the highlighted investor's) ROI.
10. The detailed log can be found in the bottom part of the screen.
11. You can control simulation speed by adjusting the `ticks` slider in the NetLogo toolbar. Moving slider to the left will slow down the simulation, while moving to the right will speed it up.


Introduction
============

There are three main agent types (or `breeds` in NetLogo speak):
- `Depositors`
- `Investors`
- `Block Producers` (BPs)


Visualization
=============

Fo the purposes of this simulation depositors are less interesting, so they're not shown in the visualization.
The main visualization shows two rings of agents:
- `Investors` (shown as a person icons). The highlighted investor is the one for whom we choose the investment strategy and measuring her ROI.
- `BPs` (shown as a block icons). BPs can be inactive (i.e. not acuumulated a minimum required stake yet) - shown as ![#808080](https://placehold.it/15/808080/000000?text=+) gray icons, or active - shown as ![#ffff00](https://placehold.it/15/ffff00/000000?text=+) yellow block icons. Slashed BPs shown as a ![#ff0000](https://placehold.it/15/ff0000/000000?text=+) red fire icons. Next to each BP icon shown annual payout in %%.


The directed links are credo loans taken by investors and delegated to BPs.

The color of the link tells which phase the credo loan is currently in:
- ![#808080](https://placehold.it/15/808080/000000?text=+) gray - loan not need to due yet
- ![#ffff00](https://placehold.it/15/ffff00/000000?text=+) yellow - grace period
- ![#00ff00](https://placehold.it/15/00ff00/000000?text=+) green - loan repaid (immediately, after grace period or after auction)
- ![#ff0000](https://placehold.it/15/ff0000/000000?text=+) red - the loan is auctioned
- ![#ffa500](https://placehold.it/15/ffa500/000000?text=+) orange - collective responsibility - not implemented


The DCR is visualized as:
- ![#0000ff](https://placehold.it/15/0000ff/000000?text=+) blue - Deposits
- ![#EE82EE](https://placehold.it/15/EE82EE/000000?text=+) violet - Capital
- ![#808080](https://placehold.it/15/808080/000000?text=+) gray - Excess Reserve (ER)

-------------------------------------------------------

# BP payouts and probability to be slashed

The average annual BP payouts (in percents) are specified as a model input.
Then each BP agent drawn a value from a normal distribution with a 10% standard deviation relative to the average.
Those with the higher payouts are assigned a higher probability to be slashed and vice versa.

BP parameters can be set as custom values or they can use predefined values taken from the existing staking networks (i.e. Livepeer, Loki, BOScoin, OKCash, PIVX, Decred, Dash, Tezos, Ethereum PoS, Cardano).

The probability to be slashed per day derived from the annual probability to be slashed using this formula:
```
P(S_daily) = 1 - (1 - P(S_annual))^(1/365)
```
where:
- `P(S_annual)` is probability to be slashed per year
- `P(S_daily)` is probability to be slashed per day

The daily interest derived from an annual compound interest using following formula (the interest here is not in percents, i.e. `1.0 = 100%`):
```
dailyInterest = (1 + annualInterest)^(1/365) - 1
```

# Investment strategy

There are 3 investment strategies:
- `random` (i.e. choose one or more random BPs)
- `low-risk` - choose one or more BPs with the lowest payouts
- `high-risk` - choose one or more BPs with the highest payouts

The amount of credos taken can be delegated to a single BP or split equally between multiple BPs (1 up to 5).

For all investor agents these parameters chosen randomly, but for highlighted `investor1` they can be specified as a model's inputs.

------------------------------------------------------

The most important parameters in the video were:
- investors choose BPs randomly
- investor1 (the one who's ROI we're measuring) always split credos between 3 BPs at a time
- credo loan term = 90 days
- min BP stake = 10000 DAI
- average annual BP staking rewards = 20% (you can see the actual payout % ones in the video)
- probability of BP to be slashed per year = 5%  
 (we have 6 BPs over 3 years ~ 1 slashing happened in the video (1/(3*6) ~ 5%)  - you can see towards the end of the log "BP 214 is slashed. Total stake lost: 50654 DAI, # credo loans recalled: 1"

You can see credos being repaid (in bold). When auctioned DET tokens only partially repay credo - it should trigger collective responsibility which it's not implemented yet.

You can see BPs switching between ACTIVE (yellow block icon) and ACCUMULATING (gray block icon) because they require a minimum BP stake. When the stake is below 10K DAI, BP will stop paying staking rewards until the minimum stake is accumulated. It happens when investors delegate credo loans to BP. When credo loans expire - the BP may switch back to ACCUMULATING.
You may see sometimes equal stakes b/c investor agents may split their credos equally between several BPs.

Notice the NetLogo log in the bottom part of the video - it describes what happens in the simulation.
The initial parameters are that BPs paying 20% on average with 5% probability to be slashed per year (with adjustment for risk - i.e. higher paying BPs slashed more, while low paying - slashed less.

You can see in the log, when BPs are slashed or credo's collateral auctioned.

-----------------------------------------

### DET/USD price graph

DET/USD price graph follows a random walk, but we can select a market bias from -2 to +2 using a vertical slider to the left of the plot:

- -2 - strong bear market
- -1 - bear market
- 0 - sideways market
- 1 - bull market
- 2 - strong bull market

You can change market bias while simulation is running, or you can set it before setup.

-------------------------------------------
