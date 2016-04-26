package client

import Service._
import Token._

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Failure, Success}

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http._
import spray.http.MediaTypes.`text/html`
import spray.httpx.unmarshalling.Unmarshaller

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

class Remambo(username: String, password: String)(implicit actorSystem: ActorSystem)
  extends Service(username, password) with YahooJapanAuctions /*with YahooJapanShopping*/ {
  implicit private val url: Uri = "https://www.remambo.jp"

  private def requestToken(implicit request: HttpRequest): Future[Try[Token]] = {
    implicit val tokenUnmarshaller = TokenUnmarshaller
    val newPipeline = gzipPipeline ~> unmarshal[Try[Token]]
    newPipeline(request)
  }

  override def authenticate: Future[Try[Cookies]] = {
    val endpoint: Uri = "auth"

    implicit val request =
      Post(uri(endpoint),
        FormData(Map(
          "set_login" -> username,
          "set_pass" -> password))
      ) ~> addHeaders(headers)

    requestAuthentication map {
      case tryCookies => tryCookies
    }
  }

  override def bid(auction_id: String, offer: Short): Future[Try[Boolean]] =
    authenticate flatMap {
      case Success(cookies) =>
        implicit val implCookies = cookies

        bidPreview(auction_id, offer) flatMap {
          case Success(token) =>
            bidPlace(auction_id, offer, token) flatMap {
              case Success(token2) =>
                bidPlace2(auction_id, offer, token2)
              case Failure(error) =>
                Future.failed(error)
            }
          case Failure(error) =>
            Future.failed(error)
        }
      case Failure(error) =>
        Future.failed(error)
    }

  /**
    * Request the preview to extract a Token with which we can place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param cookies Session cookies
    * @return The token and signature
    */
  private def bidPreview(auction_id: String, offer: Short)(implicit cookies: Cookies): Future[Try[Token]] = {
    val endpoint: Uri = "auction/bid_preview"

    implicit val request =
      Post(uri(endpoint),
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "submit_button" -> "Place bid"))
      ) ~> addHeaders(authHeaders)

    requestToken map {
      case token => token
    }
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
  private def bidPlace(auction_id: String, offer: Short, token: Token)(implicit cookies: Cookies): Future[Try[Token]] = {
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

    requestToResponse map {
      case response => claimToken(response)
    }
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
  private def bidPlace2(auction_id: String, offer: Short, token: Token)(implicit cookies: Cookies): Future[Try[Boolean]] = {
    val endpoint: Uri = "modules/yahoo_auction/data_request/rate.php"

    val finalEndpoint: Uri =
      endpoint.withQuery(Map(
        "user_rate" -> offer.toString,
        "lot_no" -> auction_id,
        "quantity" -> 1.toString,
        "token" -> token.value,
        "signature" -> token.signature))

    implicit val request = Get(uri(finalEndpoint)) ~> addHeaders(authHeaders)

    requestToResponse flatMap {
      case response => confirmBid(auction_id)
    }
  }

  override def confirmBid(auction_id: String)(implicit cookies: Cookies): Future[Try[Boolean]] =
    findAuctionInfo(auction_id, "Highest Bidder") map {
      case Success(highestBidder) =>
        Success(highestBidder == "You")
      case Failure(error) =>
        Failure(error)
    }

  override def getMinimumIncrement(auction_id: String)(implicit cookies: Cookies): Future[Try[Short]] =
    findAuctionInfo(auction_id, "Bid Step") map {
      case Success(increment) =>
        try {
          Success(increment.toShort) // toShort throws exception
        }
        catch {
          case e: Exception => Failure(e)
        }
      case Failure(error) =>
        Failure(error)
    }

  /**
    * Find the table row cell value with the desired header (on the Remambo item page)
    *
    * @param auction_id
    * @param desiredHeader The row header to search for
    * @param cookies Session cookies
    * @return The cell value associated with desiredHeader
    */
  private def findAuctionInfo(auction_id: String, desiredHeader: String)(implicit cookies: Cookies): Future[Try[String]] = {
    val endpoint: Uri = "auction/item/" + auction_id

    implicit val request = Get(uri(endpoint)) ~> addHeaders(authHeaders)

    requestToResponse map {
      case response =>
        val root = Jsoup.parse(response.entity.asString)
        val auc_info = root.select("div[id=auc_info]").select("table").select("tr")

        if (auc_info.isEmpty)
          Failure(new IllegalStateException("Failed to get " + desiredHeader))
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
  @tailrec private def findAuctionInfoRow(currentRow: Element, desiredHeader: String): Try[String] =
    if (currentRow.nextElementSibling() == null)
      Failure(new IllegalStateException("Failed to find " + desiredHeader + " row"))
    else if (currentRow.select("td[class=rightHeader skiptranslate]").text.contains(desiredHeader))
      Success(currentRow.select("td[class=skiptranslate]").text)
    else
      findAuctionInfoRow(currentRow.nextElementSibling, desiredHeader)
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
    Unmarshaller[Try[Token]](`text/html`) {
      case HttpEntity.NonEmpty(contentType, data) =>
        val root = Jsoup.parse(data.asString)
        val formInput = root.select("input")
        val token = formInput.select("input[name=token]").first().`val`
        val signature = formInput.select("input[name=signature]").first().`val`

        if (token.nonEmpty && signature.nonEmpty)
          Success(Token(token, signature))
        else
        {
          val errorMessage = root.select("div[class=alert alert-warning]").first.`val`
          Failure(new IllegalStateException(if (errorMessage.nonEmpty) errorMessage else "Failed to get bidding token"))
        }
    }

  /**
    * Extract the Token from the bid_place HTML page
    *
    * @param httpResponse
    * @return A Token
    */
  def claimToken(httpResponse: HttpResponse): Try[Token] = {
    //ie. 'var script_url = "/modules/yahoo_auction/data_request/rate.php?token=5709f3949645a&signature=87922fb277d1a987546b09f704b441aa1fde980d";'
    val script_url = httpResponse.entity.asString.split("\n").find{_.contains("script_url")}

    if (script_url.isEmpty) return Failure(new IllegalStateException("Failed to get bidding token"))

    val uri: Uri = script_url.get.split("\"")(1)

    val token = uri.query.get("token")
    val signature = uri.query.get("signature")

    if (token.isDefined && signature.isDefined)
      Success(Token(token.get, signature.get))
    else
      Failure(new IllegalStateException("Failed to get bidding token"))
  }
}