import client.Remambo
import client.Service.actorSystem

object YahooJapanShopping {
  def main(args: Array[String]): Unit = {

    val (username, password, fromUrl) = (args(0), args(1), args(2))

    val remambo = Remambo(username, password)
    remambo buyYhj fromUrl
  }
}
