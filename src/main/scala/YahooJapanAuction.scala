import client.Remambo
import client.Service.actorSystem

object YahooJapanAuction {
  def main(args: Array[String]): Unit = {

    val (username, password, auctionId, price) = (args(0), args(1), args(2), args(3))

    val remambo = Remambo(username, password)
    remambo.snipe(auctionId, price.toInt)
  }
}
