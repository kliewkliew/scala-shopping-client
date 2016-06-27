import client.Service.actorSystem

object Ebay {
  def main(args: Array[String]): Unit = {

    val (username, password, auctionId, price) = (args(0), args(1), args(2), args(3))

    val ebay = client.Ebay(username, password)
    ebay.snipe(auctionId, price.toInt)
  }
}
