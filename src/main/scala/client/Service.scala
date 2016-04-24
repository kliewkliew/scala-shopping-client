package client

import akka.actor.ActorSystem
import spray.client.pipelining._
import spray.http.HttpEncodings._
import spray.http.HttpHeaders.{Cookie, `User-Agent`, `Accept-Encoding`}
import spray.http.{HttpCookie, HttpResponse, HttpRequest, Uri}
import spray.httpx.encoding.Gzip

import scala.concurrent.duration._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * Cookies for authentication
  * Wrap a list of cookies so that they can be used as an implicit parameter
  *
  * @param cookies
  */
case class CookieWrapper(cookies: List[HttpCookie])

/**
  * Deputy service
  */
abstract class Service (username: String, password: String) {
  /**
    * Authenticate with Service
    *
    * @return A Cookie used for subsequent requests
    */
  protected def authenticate: Future[Try[CookieWrapper]]
}

// TODO: implement automatic retry in `bid`; refactor `bid` to `bidInternal`
trait Bidder extends Service {
  /**
    * Authenticate with Service and bid on an auction
    *
    * @param auction_id
    * @return true on success
    */
  def bid(auction_id: String, offer: Short): Future[Try[Boolean]]

  /**
    * Confirm a bid
    *
    * @param auction_id
    * @param unwrap Session cookies
    * @return true if you are winning the auction
    */
  def confirmBid(auction_id: String)(implicit unwrap: CookieWrapper): Future[Try[Boolean]]
}

trait Sniper extends Bidder {
  /**
    * Validate credentials and schedule a bid for 120 seconds before the auction ends
    *
    * @param auction_id
    * @return true on success
    */
  def snipe(auction_id: String, offer: Short) =
    authenticate recoverWith { case NonFatal(_) => authenticate
    } map {
      case Success(cookies: CookieWrapper) =>
        timeLeft(auction_id) map {
          case time =>
          Service.actorSystem.scheduler.scheduleOnce((time - 120).seconds) {
            bid(auction_id, offer)
          }
        }

      case Failure(error) =>
        Future.failed(error)
    }

  /**
    * Get the number of seconds until the auction ends
    *
    * @param auction_id
    * @return Number of seconds
    */
  def timeLeft(auction_id: String): Future[Int]

/*
  scheduler.s
  def receive = {
    case JobTrigger =>
      getNearbyProfiles
    case NewLocationTrigger(triggerLocation) =>
      pushLocationFuture(initSession, triggerLocation.getOrElse(location.farLocation))
  }*/
}

trait Buyer extends Service {
  /**
    * Buy a lot
    *
    * @param lot_id
    */
  def buy(lot_id: String)
}

object Service {
  /**
    * Service client Factory
    *
    * @param provider Service name
    * @return Service client instance
    */
  def apply(provider: String, username: String, password: String) =
    provider.toLowerCase match {
      case "remambo" => new Remambo(username, password)
    }

  implicit val actorSystem = ActorSystem("main")


  private[client] val userAgent =
    "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) " +
      "Chrome/49.0.2623.110 Safari/537.36 Vivaldi/1.1.443.3"

  /**
    * Construct the URI
    *
    * @param endpoint
    * @return
    */
  def uri(endpoint: Uri)(implicit url: Uri) = Uri(url + "/" + endpoint)

  private[client] val headers = List(`Accept-Encoding`(gzip), `User-Agent`(userAgent))

  private[client]  def authHeaders(implicit unwrap: CookieWrapper) = Cookie(unwrap.cookies) :: headers

  /**
    * Gzip encode a request, send it, and decode the response
    */
  private[client] val gzipPipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> decode(Gzip)

  private[client] def requestToResponse(implicit request: HttpRequest): Future[HttpResponse] = gzipPipeline(request)
}