package client

import client.Service._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http.{HttpRequest, HttpEntity, FormData, Uri}
import spray.http.MediaTypes.`text/html`
import spray.httpx.unmarshalling.Unmarshaller
import spray.json._

/**
  * APIs in "eBay developers program" do not provide bidding functionality, so I reverse-engineered the website's REST API
  *
  * @param username
  * @param password
  * @param actorSystem For Spray pipelining
  */
case class Ebay(username: String, password: String)(implicit actorSystem: ActorSystem)
  extends Service(username, password) with Sniper /*with Buyer*/ {

  // Unique item identifier
  type UIID = String

  override def authenticate: Future[Cookies] = {
    val endpoint: Uri = "https://signin.ebay.com/ws/eBayISAPI.dll"

    implicit val request =
      Post(endpoint,
        FormData(Map(
          "userid" -> username,
          "pass" -> password,
          "MfcISAPICommand" -> "SignInWelcome"))
      ) ~> addHeaders(headers)

    requestAuthentication
  }

  override def bid(auction_id: String, offer: Int): Future[Boolean] =
    authenticate flatMap { cookies =>
      implicit val implCookies = cookies

      bidPreview(auction_id) flatMap { uiid =>
          bidPlace(auction_id, offer, uiid)
      }
    }

  /**
    * Request the preview to extract a UIID with which we can place a bid
    *
    * @param auction_id
    * @param cookies Session cookies
    * @return The token and signature
    */
  private def bidPreview(auction_id: String)(implicit cookies: Cookies): Future[UIID] = {
    val endpoint: Uri = "itm/" + auction_id

    implicit val request = Get(endpoint) ~> addHeaders(authHeaders)

    requestUiid
  }

  /**
    * Place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param uiid Unique item identifier
    * @param cookies Session cookies
    * @return true on success
    */
  private def bidPlace(auction_id: String, offer: Int, uiid: UIID)(implicit cookies: Cookies): Future[Boolean] = {
    val endpoint: Uri = "https://offer.ebay.com/ws/eBayISAPI.dll"

    val finalEndpoint =
      endpoint.withQuery("MakeQuickBid")
      .withQuery(Map(
        "item" -> auction_id,
        "uiid" -> uiid,
        "maxbid" -> offer.toString,
        "f" -> "json",
        "flow" -> "bm",
        "mode" -> "1"))

    implicit val request = Get(finalEndpoint) ~> addHeaders(authHeaders)

    requestToResponse flatMap { response =>
      confirmBid(auction_id)
    }
  }

  override def confirmBid(auction_id: String)(implicit cookies: Cookies): Future[Boolean] =
    findAuctionInfo(auction_id, "ViewerItemRelation")
      .map(_.toString.toUpperCase == "HIGHBIDDER")

  override def timeLeft(auction_id: String): Future[Int] =
    authenticate flatMap { cookies =>
      implicit val implCookies = cookies
      findAuctionInfo(auction_id, "TimeLeft") map { time =>
        implicit class JsonTimeConverter(jsValue: Option[JsValue]) {
          def getInt: Int = jsValue.get.asJsObject.toString.toInt
        }

        val days = time.fields.get("DaysLeft").getInt
        val hours = time.fields.get("HoursLeft").getInt
        val minutes = time.fields.get("MinutesLeft").getInt
        val seconds = time.fields.get("SecondsLeft").getInt

        days * 86400
        + hours * 3600
        + minutes * 60
        + seconds
      }
    }

  /**
    * Extract the UUID from the item page
    *
    * @return A UIID
    */
  implicit val UuidUnmarshaller =
    Unmarshaller[UIID](`text/html`) {
      case HttpEntity.NonEmpty(contentType, data) =>
        val line = data.asString.split("\n").find(_.substring(0, 8) == "$rwidgets")

        val uiid = new Regex("""uiid=-?\d+""").findFirstIn(line.getOrElse(""))
          .getOrElse("").replaceAll("uiid=", "")

        if(uiid.nonEmpty)
          uiid
        else
          throw new IllegalStateException("Failed to get UIID")
    }

  private def requestUiid(implicit request: HttpRequest): Future[UIID] = {
    val newPipeline = gzipPipeline ~> unmarshal[UIID]
    newPipeline(request)
  }

  /**
    * Find the JSON value with the desired key (from the eBay auction details)
    *
    * @param auction_id
    * @param desiredKey The key to search for
    * @param cookies Session cookies
    * @return The cell value associated with desiredHeader
    */
  private def findAuctionInfo(auction_id: String, desiredKey: String)(implicit cookies: Cookies): Future[JsObject] = {
    val endpoint: Uri = "https://offer.ebay.com/ws/eBayISAPI.dll"

    val finalEndpoint =
      endpoint.withQuery("MakeQuickBid")
        .withQuery(Map(
          "item" -> auction_id,
          "maxbid" -> "0",
          "f" -> "json",
          "flow" -> "bm"))

    implicit val request = Get(finalEndpoint) ~> addHeaders(authHeaders)

    requestToResponse map { response =>
      val auctionInfo = response.entity.asString.parseJson.asJsObject.fields.get(desiredKey)

      if (auctionInfo.nonEmpty)
        auctionInfo.get.asJsObject
      else
        throw new IllegalStateException("Failed to get " + desiredKey + " auction info")
    }
  }

}
