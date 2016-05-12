Scala Client for Shopping Services
==============

This client currently adds sniper capabilities (bidding right before the auction ends instead of exposing your bid early) for shopping services that do not provide sniping functioonality.

A possible enhancement is to watch the price of a store (not auction) item and auto-buy below a threshold.
One could also extend eBay by adding the ability to bid conditionally, with more features than you would find in a typical sniper service.

I am writing this client so that I can snipe auctions using the Remambo deputy service (to buy from Yahoo! Japan; nobody uses eBay in Japan), which differs from other services by offering upfront charges, a two month storage period (twice as long as usual), and allowing you to bid up to *20x* your deposit (most allow 1x deposit).

If you would like to register, you can use my affiliate link :)
`https://www.remambo.jp/?auc7531`

## Clients
###eBay
WIP, will be tested when I find something to buy :)
APIs in "eBay developers program" do not provide bidding functionality, so I reverse-engineered the website's REST API.

###Remambo (for Yahoo! Japan Auctions; eventually for Yahoo! Japan Shopping and Rakuten)
Reverse-engineered REST API.

## Usage
An example is available in `Application.scala`.

## Implementation Details
Http requests execute asynchronously using `Future`s and the `Spray` framework. Clients are composed of mixin `trait`s. 
