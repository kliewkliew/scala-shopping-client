package client

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success}
import scala.xml.{Node, NodeSeq}

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http._
import spray.httpx.encoding.Gzip
import spray.http.HttpEncodings._
import spray.http.HttpHeaders._

class Remambo extends Service with Sniper /*with Buyer*/ {
  private val url = "https://www.remambo.jp"
  implicit private val actorSystem = ActorSystem("forPipeline")

  /**
    * Construct the URI
    * @param endpoint
    * @return
    */
  private def uri(implicit endpoint: String) = Uri(url + "/" + endpoint)

  /**
    * Gzip encode a request, send it, and decode the response
    */
  val gzipPipeline: HttpRequest => Future[HttpResponse] = encode(Gzip) ~> sendReceive ~> decode(Gzip)

  private def requestToResponse(implicit request: HttpRequest): Future[HttpResponse] = gzipPipeline(request)

  private def requestToResponseXML(implicit request: HttpRequest): Future[NodeSeq] = {
    val newPipeline = gzipPipeline ~> unmarshal[NodeSeq]
    newPipeline(request)
  }

  override def authenticate(implicit credentials: Credentials): Future[Try[CookieWrapper]] = {
    implicit val endpoint = "auth"

    implicit val request =
      Post(uri,
        FormData(Map(
          "set_login" -> credentials.username,
          "set_pass" -> credentials.password))
      ) ~> addHeader(`Accept-Encoding`(gzip))
      // add Content-Type header? or does the pipeline take care of that?

    requestToResponse map {
      case response =>
        val cookies = response.headers.collect{ case `Set-Cookie`(cookie) => cookie }

        if (cookies.nonEmpty)
          Success(CookieWrapper(cookies))
        else
          Failure(new IllegalStateException("Failed to authenticate"))
    }
  }

  override def bid(auction_id: String, offer: Short)(implicit credentials: Credentials): Future[Try[Boolean]] =
    authenticate recoverWith { case NonFatal(_) => authenticate
    } flatMap {
      case Success(cookies: CookieWrapper) =>
        implicit val implCookies = cookies

        bidPreview(auction_id, offer) recoverWith { case NonFatal(_) => bidPreview(auction_id, offer)
        } flatMap {
          case Success(token: Token) =>
            bidPlace(auction_id, offer, token) recoverWith {
              case NonFatal(_) => bidPlace(auction_id, offer, token)
            }
          case Failure(error) =>
            Future.failed(error)
        }

      case Failure(error) =>
        Future.failed(error)
    }

  /**
    * Request the preview to extract a Token with which we can place a bid
    * @param auction_id
    * @param offer Price offer
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return The token and signature
    */
  private def bidPreview(auction_id: String, offer: Short)(implicit unwrap: CookieWrapper): Future[Try[Token]] = {
    implicit val endpoint = "auction/bid_preview"

    implicit val request =
      (Post(uri,
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "submit_button" -> "Place bid"))
      ) ~> addHeader(`Accept-Encoding`(gzip))
        ~> addHeader(Cookie(unwrap.cookies)))
    // add Content-Type header? or does the pipeline take care of that?

    requestToResponseXML map {
      case token => token // implicit conversion
    }
  }

  /**
    * Place a bid
    * @param auction_id
    * @param offer Price offer
    * @param token Token used to validate a bid
    * @param unwrap Session cookies wrapped in a case class because Try cannot distinguish between different types of List
    * @return
    */
  private def bidPlace(auction_id: String, offer: Short, token: Token)(implicit unwrap: CookieWrapper): Future[Try[Boolean]] = {
    implicit val endpoint = "auction/bid_place"

    implicit val request =
      (Post(uri,
        FormData(Map(
          "user_rate" -> offer.toString,
          "lot_no" -> auction_id,
          "quantity" -> 1.toString,
          "token" -> token.value,
          "signature" -> token.signature))
      ) ~> addHeader(`Accept-Encoding`(gzip))
        ~> addHeader(Cookie(unwrap.cookies)))
    // add Content-Type header? or does the pipeline take care of that?

    requestToResponse map {
      case response =>
      // TODO: verify bid and bidder, and was not outbid
      Success(true)
    }
  }
}

/**
  * Wrap a list of cookies so that they can be used as an implicit parameter
  * @param cookies
  */
sealed case class CookieWrapper(cookies: List[HttpCookie]) extends Cookie

/**
  * The token used to place a bid
  * @param value
  * @param signature
  */
sealed case class Token(value: String, signature: String)

object Token {
  /**
    * Extract the Token from the bid_preview HTML page
    * @param response
    * @return A Token
    */
  implicit def claimToken(response: NodeSeq): Try[Token] = {
    val formInput = response \\ "input"
    val token = formInput find nodeMatchingId("token") map nodeValue
    val signature = formInput find nodeMatchingId("signature") map nodeValue

    if (token.isDefined && signature.isDefined)
      Success(Token(token.get, signature.get))
    else
      Failure(new IllegalStateException("Failed to get bidding token"))
  }

  /**
    * Determine whether a Node has a matching "id" attribute
    * @param id
    * @return True if the Node matches
    */
  private def nodeMatchingId(id: String) = {node: Node => (node \ "@id").text == id}

  /**
    * Get the text from the "value" attribute of a Node
    * @return Text of the "value" attribute
    */
  private def nodeValue = {node: Node => (node \ "@value").text}
}