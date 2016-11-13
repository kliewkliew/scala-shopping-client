package client

import akka.actor.ActorSystem

import spray.client.pipelining._
import spray.http.HttpEncodings.gzip
import spray.http.HttpHeaders.{`Set-Cookie`, Cookie, `User-Agent`, `Accept-Encoding`}
import spray.http._
import spray.httpx.encoding.Gzip
import spray.httpx.unmarshalling.{MalformedContent, Deserialized, FromResponseUnmarshaller}

import scala.concurrent.duration._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Cookies for authentication
  * Wrap a list of cookies so that they can be used as an implicit parameter
  *
  * @param cookies
  */
case class Cookies(cookies: List[HttpCookie])

/**
  * Deputy service
  */
abstract class Service (username: String, password: String) {
  /**
    * Authenticate with Service
    *
    * @return A Cookie used for subsequent requests
    */
  protected def authenticate: Future[Cookies]
}

// TODO: implement automatic retry in `bid`; refactor existing `bid` to `bidInternal`
trait Bidder extends Service {
  /**
    * Authenticate with Service and bid on an auction
    *
    * @param auction_id
    * @param offer Bidding price. Currency is dependant on the service
    * @return true on success
    */
  def bid(auction_id: String, offer: Int): Future[Boolean]

  /**
    * Confirm a bid
    *
    * @param auction_id
    * @param cookies Session cookies
    * @return true if you are winning the auction
    */
  def confirmBid(auction_id: String)(implicit cookies: Cookies): Future[Boolean]
}

trait Sniper extends Bidder {
  //TODO: take actorRef or lambda/callback param to execute when the bid Future returns
  /**
    * Validate credentials and schedule a bid for 120 seconds before the auction ends
    *
    * @param auction_id
    * @param offer Bidding price. Currency is dependant on the service
    * @return true on success
    */
  def snipe(auction_id: String, offer: Int) =
  authenticate map { cookies =>
    timeLeft(auction_id)
      .map { time =>
        Service.actorSystem.scheduler.scheduleOnce((time - 120).seconds) {
          bid(auction_id, offer)
        }
      }
      .recoverWith { case _ =>
        bid(auction_id, offer)
      }
  }

  /**
    * Get the number of seconds until the auction ends
    *
    * @param auction_id
    * @return Number of seconds
    */
  def timeLeft(auction_id: String): Future[Int]
}

/**
  * Item information used for purchasing
  * @param name
  * @param price
  * @param url
  */
case class ItemInfo(name: String, price: Int, url: Uri)

trait Shopper extends Service {
  /**
    * Buy something through this deputy
    * @param itemInfo
    * @return
    */
  protected def buy(itemInfo: ItemInfo): Future[Boolean]
}

object Service {
  implicit val actorSystem = ActorSystem("main")

  /**
    * Service client Factory
    *
    * @param provider Service name
    * @return Service client instance
    */
  def apply(provider: String, username: String, password: String) =
  provider.toLowerCase match {
    case "ebay"     => Ebay(username, password)
    case "remambo"  => Remambo(username, password)
  }

  private[client] val userAgent =
    "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) " +
      "Chrome/49.0.2623.110 Safari/537.36 Vivaldi/1.1.443.3"

  /**
    * Construct the URI
    *
    * @param endpoint Endpoint relative to the base url
    * @param url Base url
    * @return Unique resource identifier
    */
  def uri(endpoint: Uri)(implicit url: Uri) = Uri(url + "/" + endpoint)

  private[client] val headers = List(`Accept-Encoding`(gzip), `User-Agent`(userAgent))

  private[client] def authHeaders(implicit unwrap: Cookies) = Cookie(unwrap.cookies) :: headers

  /**
    * Gzip encode a request, send it, and decode the response
    */
  private[client] val gzipPipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> decode(Gzip)

  private[client] def requestToResponse(implicit request: HttpRequest): Future[HttpResponse] = gzipPipeline(request)

  private[client] def requestAuthentication(implicit request: HttpRequest): Future[Cookies] = {
    implicit val tokenUnmarshaller = CookiesUnmarshaller
    val newPipeline = gzipPipeline ~> unmarshal[Cookies]
    newPipeline(request)
  }

  /**
    * Unmarshal Cookies from the authentication headers
    *
    * @return Cookies
    */
  private val CookiesUnmarshaller = new FromResponseUnmarshaller[Cookies] {
    def apply(httpResponse: HttpResponse): Deserialized[Cookies] = {
      val cookies = httpResponse.headers.collect { case `Set-Cookie`(cookie) => cookie }

      if (cookies.nonEmpty)
        Right(Cookies(cookies))
      else
        Left(MalformedContent("Failed to authenticate", new IllegalStateException("Failed to authenticate")))
    }
  }
}