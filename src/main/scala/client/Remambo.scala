package client

import Service._
import Token._

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http._
import spray.http.MediaTypes.`text/html`
import spray.httpx.unmarshalling.Unmarshaller

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

case class Remambo(username: String, password: String)(implicit actorSystem: ActorSystem)
  extends Service(username, password) with YahooJapanAuctions with YahooJapanShopping with Rakuten {
  implicit private val url: Uri = "https://www.remambo.jp"

  private def requestToken(implicit request: HttpRequest): Future[Token] = {
    implicit val tokenUnmarshaller = TokenUnmarshaller
    val newPipeline = gzipPipeline ~> unmarshal[Token]
    newPipeline(request)
  }

  override def authenticate: Future[Cookies] = {
    val endpoint: Uri = "auth"

    implicit val request =
      Post(uri(endpoint),
        FormData(Map(
          "set_login" -> username,
          "set_pass" -> password))
      ) ~> addHeaders(headers)

    requestAuthentication
  }

  override def bid(auction_id: String, offer: Int): Future[Boolean] = {
    authenticate flatMap { cookies =>
      implicit val implCookies = cookies

      val token = bidPreview(auction_id, offer)
      val token2 = token flatMap (bidPlace(auction_id, offer, _))
      val bidResult = token2 flatMap (bidPlace2(auction_id, offer, _))

      bidResult
    }
  }

  /**
    * Request the preview to extract a Token with which we can place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param cookies Session cookies
    * @return The token and signature
    */
  private def bidPreview(auction_id: String, offer: Int)(implicit cookies: Cookies): Future[Token] = {
    val endpoint: Uri = "auction/bid_preview"

    implicit val request =
      Post(uri(endpoint),
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "submit_button" -> "Place bid"))
      ) ~> addHeaders(authHeaders)

    requestToken
  }

  /**
    * Get the second confirmation Token to place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param token Token used to validate a bid
    * @param cookies Session cookies
    * @return true on success
    */
  private def bidPlace(auction_id: String, offer: Int, token: Token)(implicit cookies: Cookies): Future[Token] = {
    val endpoint: Uri = "auction/bid_place"

    implicit val request =
      Post(uri(endpoint),
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "token" -> token.value,
          "signature" -> token.signature,
          "make" -> "Confirm Bid"))
      ) ~> addHeaders(authHeaders)

    requestToResponse map claimToken
  }

  /**
    * Place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param token Token used to validate a bid
    * @param cookies Session cookies
    * @return true on success
    */
  private def bidPlace2(auction_id: String, offer: Int, token: Token)(implicit cookies: Cookies): Future[Boolean] = {
    val endpoint: Uri = "modules/yahoo_auction/data_request/rate.php"

    val finalEndpoint: Uri =
      endpoint.withQuery(Map(
        "user_rate" -> offer.toString,
        "lot_no" -> auction_id,
        "quantity" -> 1.toString,
        "token" -> token.value,
        "signature" -> token.signature))

    implicit val request = Get(uri(finalEndpoint)) ~> addHeaders(authHeaders)

    requestToResponse flatMap { response =>
      confirmBid(auction_id)
    }
  }

  override def confirmBid(auction_id: String)(implicit cookies: Cookies): Future[Boolean] =
    findAuctionInfo(auction_id, "Highest Bidder").map(_ == "You")

  override def getMinimumIncrement(auction_id: String)(implicit cookies: Cookies): Future[Short] =
    findAuctionInfo(auction_id, "Bid Step").map(_.toShort)

  /**
    * Find the table row cell value with the desired header (on the Remambo item page)
    *
    * @param auction_id
    * @param desiredHeader The row header to search for
    * @param cookies Session cookies
    * @return The cell value associated with desiredHeader
    */
  private def findAuctionInfo(auction_id: String, desiredHeader: String)(implicit cookies: Cookies): Future[String] = {
    val endpoint: Uri = "auction/item/" + auction_id

    implicit val request = Get(uri(endpoint)) ~> addHeaders(authHeaders)

    requestToResponse map { response =>
      val root = Jsoup.parse(response.entity.asString)
      val auc_info = root.select("div[id=auc_info]").select("table").select("tr")

      if (auc_info.isEmpty)
        throw new IllegalStateException("Failed to get " + desiredHeader)
      else
        findAuctionInfoRow(auc_info.first, desiredHeader)
    }
  }

  /**
    * Find the table row cell value with the desired header
    * We cannot use Scala collections in JSoup so lets use tail recursion because it is more functional
    *
    * @param currentRow Next table row to be checked
    * @param desiredHeader The row header to search for
    * @return The cell value associated with desiredHeader
    */
  @tailrec private def findAuctionInfoRow(currentRow: Element, desiredHeader: String): String =
  if (currentRow.nextElementSibling() == null)
    throw new IllegalStateException("Failed to find " + desiredHeader + " row")
  else if (currentRow.select("td[class=rightHeader skiptranslate]").text.contains(desiredHeader))
    currentRow.select("td[class=skiptranslate]").text
  else
    findAuctionInfoRow(currentRow.nextElementSibling, desiredHeader)

  override protected def buy(itemInfo: ItemInfo) =
    authenticate flatMap { cookies =>
      implicit val implCookies = cookies

      val endpoint: Uri = "https://www.remambo.jp/shoppingcart"

      val finalEndpoint: Uri =
        endpoint.withQuery(Map(
          "title" -> itemInfo.name,
          "url" -> itemInfo.url.toString,
          "price" -> itemInfo.price.toString,
          "shipping" -> "0",
          "step_2" -> "1",
          "payment_method" -> "1",
          "qty" -> "1"))

      implicit val request = Get(uri(finalEndpoint)) ~> addHeaders(authHeaders)

      requestToResponse map { response =>
        val root = Jsoup.parse(response.entity.asString)
        root.select("body").text().contains("Your order is accepted")
      }
    }
}

/**
  * The token used to place a bid
  *
  * @param value
  * @param signature
  */
sealed case class Token(value: String, signature: String)

object Token {
  /**
    * Unmarshal the Token from the bid_preview HTML page
    *
    * @return A Token
    */
  val TokenUnmarshaller =
  Unmarshaller[Token](`text/html`) {
    case HttpEntity.NonEmpty(contentType, data) =>
      val root = Jsoup.parse(data.asString)
      val formInput = root.select("input")
      val token = formInput.select("input[name=token]").first().`val`
      val signature = formInput.select("input[name=signature]").first().`val`

      if (token.nonEmpty && signature.nonEmpty)
        Token(token, signature)
      else
      {
        val errorMessage = root.select("div[class=alert alert-warning]").first.`val`
        throw new IllegalStateException(if (errorMessage.nonEmpty) errorMessage else "Failed to get bidding token")
      }
  }

  /**
    * Extract the Token from the bid_place HTML page
    *
    * @param httpResponse
    * @return A Token
    */
  def claimToken(httpResponse: HttpResponse): Token = {
    //ie. 'var script_url = "/modules/yahoo_auction/data_request/rate.php?token=5709f3949645a&signature=87922fb277d1a987546b09f704b441aa1fde980d";'
    val script_url = httpResponse.entity.asString.split("\n").find{_.contains("script_url")}

    if (script_url.isEmpty) throw new IllegalStateException("Failed to get bidding token")

    val uri: Uri = script_url.get.split("\"")(1)

    val token = uri.query.get("token")
    val signature = uri.query.get("signature")

    if (token.isDefined && signature.isDefined)
      Token(token.get, signature.get)
    else
      throw new IllegalStateException("Failed to get bidding token")
  }
}