package client

import Service._
import org.jsoup.Jsoup

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import spray.client.pipelining._
import spray.http.Uri

trait Rakuten extends Service {
  implicit private val url: Uri = "https://www.rakuten.co.jp"
  /**
    * Get the price of the item and buy it
    *
    * @param item_url
    */
  def buyRakuten(item_url: Uri): Future[Try[Boolean]] = {
    getPrice(item_url) flatMap {
      case Success(price) =>
        buyRakutenInternal(item_url, price)
      case Failure(e) =>
        Future.failed(e)
    }
  }

  /**
    * Buy a lot
    *
    * @param item_url
    */
  protected def buyRakutenInternal(item_url: Uri, price: Int): Future[Try[Boolean]]

  /**
    * Get the price of an item
    * @param item_url
    * @return
    */
  def getPrice(item_url: Uri): Future[Try[Int]] = {
    implicit val request = Get(item_url) ~> addHeaders(headers)

    requestToResponse map { response =>
        try {
          val root = Jsoup.parse(response.entity.asString)
          val price = root.select("body").select("div[id=pagebody]").select("table[id=rakutenLimitedId_cart]")
          .select("span[class=price2]").text().replaceAll("円", "").replaceAll(",", "").toInt
          Success(price)
        }
        catch {
          case e: Exception => Failure(e)
        }

    }

  }
}
