package client

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * Cookie for authentication
  */
protected abstract class Cookie

/**
  * Deputy service
  */
abstract class Service (username: String, password: String) {
  /**
    * Authenticate with Service
    * @return A Cookie used for subsequent requests
    */
  protected def authenticate: Future[Try[Cookie]]
}

trait Bidder extends Service {
  /**
    * Authenticate with Service and bid on an auction
    * @param auction_id
    * @return 0 for success
    */
  def bid(auction_id: String, offer: Short): Future[Try[Boolean]]
}

trait Sniper extends Bidder {
  /**
    * Validate credentials and schedule a bid
    * @param auction_id
    * @return 0 for success
    */
  def snipe(auction_id: String, offer: Short): Future[Try[Boolean]] =
    authenticate recoverWith { case NonFatal(_) => authenticate
    } flatMap {
      case Success(cookies: Cookie) =>
        // TODO: *actually* schedule a bid
        // TODO: incremental bidding
        bid(auction_id, offer)
      case Failure(error) =>
        Future.failed(error)
    }
}

trait Buyer extends Service {
  /**
    * Buy a lot
    * @param lot_id
    */
  def buy(lot_id: String)
}

object Service {
  /**
    * Service client Factory
    * @param provider Service name
    * @return Service client instance
    */
  def apply(provider: String, username: String, password: String) =
    provider.toLowerCase match {
      case "remambo" => new Remambo(username, password)
    }
}