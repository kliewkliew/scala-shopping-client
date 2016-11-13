package client

import Service._
import java.util.Date

import org.jsoup.Jsoup

import spray.client.pipelining._
import spray.http._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait YahooJapanAuctions extends Sniper {
  implicit private val url: Uri = "http://page2.auctions.yahoo.co.jp"

  override def timeLeft(auction_id: String): Future[Int] = {
    val endpoint: Uri = "now"

    val finalEndpoint: Uri =
      endpoint.withQuery(Map(
        "aID" -> auction_id,
        "nowtime" -> new Date().getTime.toString))

    implicit val request = Get(uri(finalEndpoint)) ~> addHeaders(headers)

    requestToResponse map { response =>
      response.entity.asString.toInt
    }
  }

  /**
    * Bid at the minimum increment
    *
    * @param auction_id
    * @param offer
    */
  def incrementalSnipe(auction_id: String, offer: Int) = {
    // TODO
  }

  /**
    * Get the minimum increment in bid. Not exposed by YHJ API (since we can't login to YHJ directly)
    * so get this from the deputy service.
    *
    * @param auction_id
    * @param cookies Session cookies
    * @return
    */
  def getMinimumIncrement(auction_id: String)(implicit cookies: Cookies): Future[Short]
}

trait YahooJapanShopping extends Shopper {
  implicit private val url: Uri = "http://store.shopping.yahoo.co.jp"

  /**
    * Get the price of the item and buy it
    *
    * @param item_url
    */
  def buyYhj(item_url: Uri): Future[Boolean] =
  getItemInfo(item_url) flatMap buy

  private def getItemInfo(item_url: Uri): Future[ItemInfo] = {
    implicit val request = Get(item_url) ~> addHeaders(headers)

    requestToResponse map { response =>
      val root = Jsoup.parse(response.entity.asString)
      val name = root.select("title").text()
      val price = root.select("meta[itemprop=price]").attr("content").toInt

      ItemInfo(name, price, item_url)
    }
  }
}
