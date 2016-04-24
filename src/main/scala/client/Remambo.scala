package client

import Token._
import org.jsoup.nodes.Element

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success}

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http._
import spray.http.HttpEncodings._
import spray.http.HttpHeaders._
import spray.http.MediaTypes._
import spray.httpx.encoding.Gzip
import spray.httpx.unmarshalling._

import org.jsoup.Jsoup

class Remambo(username: String, password: String) extends Service(username, password) with Sniper /*with Buyer*/ {
  private val url = "https://www.remambo.jp"
  implicit private val actorSystem = ActorSystem("forPipeline")
  private val userAgent =
    "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) " +
      "Chrome/49.0.2623.110 Safari/537.36 Vivaldi/1.1.443.3"

  /**
    * Construct the URI
    *
    * @param endpoint
    * @return
    */
  private def uri(implicit endpoint: Uri) = Uri(url + "/" + endpoint)

  private val headers = List(`Accept-Encoding`(gzip), `User-Agent`(userAgent))

  private def authHeaders(implicit unwrap: CookieWrapper) = Cookie(unwrap.cookies) :: headers

  /**
    * Gzip encode a request, send it, and decode the response
    */
  val gzipPipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> decode(Gzip)

  private def requestToResponse(implicit request: HttpRequest): Future[HttpResponse] = gzipPipeline(request)

  private def requestToToken(implicit request: HttpRequest): Future[Try[Token]] = {
    implicit val tokenUnmarshaller = TokenUnmarshaller
    val newPipeline = gzipPipeline ~> unmarshal[Try[Token]]
    newPipeline(request)
  }

  override def authenticate: Future[Try[CookieWrapper]] = {
    implicit val endpoint: Uri = "auth"

    implicit val request =
      Post(uri,
        FormData(Map(
          "set_login" -> username,
          "set_pass" -> password))
      ) ~> addHeaders(headers)

    requestToResponse map {
      case response =>
        val cookies = response.headers.collect{ case `Set-Cookie`(cookie) => cookie }

        if (cookies.nonEmpty)
          Success(CookieWrapper(cookies))
        else
          Failure(new IllegalStateException("Failed to authenticate"))
    }
  }

  override def bid(auction_id: String, offer: Short): Future[Try[Boolean]] =
    authenticate recoverWith { case NonFatal(_) => authenticate
    } flatMap {
      case Success(cookies: CookieWrapper) =>
        implicit val implCookies = cookies

        bidPreview(auction_id, offer) recoverWith { case NonFatal(_) => bidPreview(auction_id, offer)
        } flatMap {
          case Success(token: Token) =>
            bidPlace(auction_id, offer, token) recoverWith {case NonFatal(_) => bidPlace(auction_id, offer, token)
            } flatMap {
              case Success(token2) =>
                bidPlace2(auction_id, offer, token2) recoverWith { case NonFatal(_) => bidPlace2(auction_id, offer, token2)}
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
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return The token and signature
    */
  private def bidPreview(auction_id: String, offer: Short)(implicit unwrap: CookieWrapper): Future[Try[Token]] = {
    implicit val endpoint: Uri = "auction/bid_preview"

    implicit val request =
      Post(uri,
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "submit_button" -> "Place bid"))
      ) ~> addHeaders(authHeaders)

    requestToToken map {
      case token => token
    }
  }

  /**
    * Get the second confirmation Token to place a bid
    *
    * @param auction_id
    * @param offer Price offer
    * @param token Token used to validate a bid
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return true on success
    */
  private def bidPlace(auction_id: String, offer: Short, token: Token)(implicit unwrap: CookieWrapper): Future[Try[Token]] = {
    implicit val endpoint: Uri = "auction/bid_place"

    implicit val request =
      Post(uri,
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
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return true on success
    */
  private def bidPlace2(auction_id: String, offer: Short, token: Token)(implicit unwrap: CookieWrapper): Future[Try[Boolean]] = {
    val endpoint: Uri = "modules/yahoo_auction/data_request/rate.php"

    implicit val finalEndpoint: Uri =
      endpoint.withQuery(Map(
        "user_rate" -> offer.toString,
        "lot_no" -> auction_id,
        "quantity" -> 1.toString,
        "token" -> token.value,
        "signature" -> token.signature))

    implicit val request = Get(uri) ~> addHeaders(authHeaders)

    requestToResponse flatMap  {
      case response => confirmBid(auction_id)
    }
  }

  /**
    * Confirm a bid
    *
    * @param auction_id
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return true if you are winning the auction
    */
  private def confirmBid(auction_id: String)(implicit unwrap: CookieWrapper): Future[Try[Boolean]] = {
    implicit val endpoint: Uri = "auction/item/" + auction_id

    implicit val request = Get(uri) ~> addHeaders(authHeaders)

    requestToResponse map {
      case response =>
        val root = Jsoup.parse(response.entity.asString)
        val auc_info = root.select("div[id=auc_info]").select("table").select("tr")

        if (auc_info.isEmpty)
          Failure(new IllegalStateException("Failed to confirm bid"))
        else
          Success(findAuctionInfo(auc_info.first, "Highest Bidder") == "You")
    }
  }

  /**
    * Find the table row with the desired header
    * We cannot use Scala collections in JSoup so lets use tail recursion because it is more functional
    *
    * @param currentRow Next table row to be checked
    * @param desiredHeader The row header to search for
    * @return The value associated with desiredHeader
    */
  @tailrec private def findAuctionInfo(currentRow: Element, desiredHeader: String): String = {
    if (currentRow.select("td[class=rightHeader skiptranslate]").text.contains(desiredHeader))
      currentRow.select("td[class=skiptranslate]").text
    else
      findAuctionInfo(currentRow.nextElementSibling, desiredHeader)
  }
}

/**
  * Wrap a list of cookies so that they can be used as an implicit parameter
  *
  * @param cookies
  */
sealed case class CookieWrapper(cookies: List[HttpCookie]) extends Cookie

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

    if (script_url.isEmpty) Failure(new IllegalStateException("Failed to get bidding token"))

    val uri: Uri = script_url.get.split("\"")(1)

    val token = uri.query.get("token")
    val signature = uri.query.get("signature")

    if (token.isDefined && signature.isDefined)
      Success(Token(token.get, signature.get))
    else
      Failure(new IllegalStateException("Failed to get bidding token"))
  }
}