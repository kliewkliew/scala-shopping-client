package client

import Service._

import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global

import spray.client.pipelining._
import spray.http._

import scala.concurrent.Future
import scala.util.Try

trait YahooJapanAuctions extends Sniper {
  implicit private val url: Uri = "http://page2.auctions.yahoo.co.jp"

  override def timeLeft(auction_id: String): Future[Int] = {
    val endpoint: Uri = "now"

    val finalEndpoint: Uri =
      endpoint.withQuery(Map(
        "aID" -> auction_id,
        "nowtime" -> (new Date().getTime / 1000).toString))

    implicit val request = Get(uri(endpoint)) ~> addHeaders(headers)

    requestToResponse map  {
      case response => response.entity.asString.toShort
    }
  }

  /**
    * Bid at the minimum increment
    *
    * @param auction_id
    * @param offer
    */
  def incrementalSnipe(auction_id: String, offer: Short) = {
    // TODO
  }

  /**
    * Get the minimum increment in bid
    *
    * @param auction_id
    */
  def getMinimumIncrement(auction_id: String)(implicit unwrap: CookieWrapper): Future[Try[Short]]
}

trait YahooJapanShopping extends Buyer {
  implicit private val url: Uri = "http://store.shopping.yahoo.co.jp"
  override def buy(lot_id: String) = {
    // TODO
  }

  /**
    * A separate buy so that we can have a client mixin both YahooJapanShopping and Rakuten (which both extend Buyer)
    *
    * @param lot_id
    */
  def buyYhj(lot_id: String)
}
