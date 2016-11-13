package client

import Service._
import org.jsoup.Jsoup

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import spray.client.pipelining._
import spray.http.Uri

trait Rakuten extends Shopper {
  implicit private val url: Uri = "https://www.rakuten.co.jp"
  /**
    * Get the price of the item and buy it
    *
    * @param item_url
    */
  def buyRakuten(item_url: Uri): Future[Try[Boolean]] = {
    getItemInfo(item_url) flatMap {
      case Success(itemInfo) =>
        buy(itemInfo)
      case Failure(e) =>
        Future.failed(e)
    }
  }

  /**
    * Get the price of an item
    * @param item_url
    * @return
    */
  private def getItemInfo(item_url: Uri): Future[Try[ItemInfo]] = {
    implicit val request = Get(item_url) ~> addHeaders(headers)

    requestToResponse map { response =>
      val root = Jsoup.parse(response.entity.asString)
      val name = root.select("title").text()
      val priceT = Try(root.select("body").select("div[id=pagebody]").select("table[id=rakutenLimitedId_cart]")
        .select("span[class=price2]").text().replaceAll("円", "").replaceAll(",", "").toInt)

      priceT.map(ItemInfo(name, _, item_url))
    }
  }
}
