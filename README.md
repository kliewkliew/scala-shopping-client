Sniper Client for Japan Deputy Shopping Services
==============

This client adds sniper capabilities (bidding right before the auction ends instead of exposing your bid early) for the deputy services that do not provide sniping services.
Another possible feature is to watch the price of a store (not auction) item and auto-buy below a threshold.
One could also extend eBay by adding the ability to bid conditionally, with more features than you would find in a typical sniper service.

I am writing this client so that I can snipe auctions using the Remambo deputy service, which differs from other services by offering upfront charges, a two month storage period (twice as long as usual), and allowing you to bid up to *20x* your deposit (most allow 1x deposit).

If you would like to register, you can use my affiliate link :)
`https://www.remambo.jp/?auc7531`

## Implementation Details
Http requests execute asynchronously using `Future`s and the `Spray` library. Clients are composed of mixin `trait`s. 
