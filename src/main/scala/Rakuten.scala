import client.Remambo
import client.Service.actorSystem

import scala.concurrent.ExecutionContext.Implicits.global

object Rakuten {
  def main(args: Array[String]): Unit = {

    val (username, password, fromUrl) = (args(0), args(1), args(2))

    val remambo = Remambo(username, password)
    remambo buyRakuten fromUrl map {
      case true =>
        println("Success")
        sys.exit(0)
      case false =>
        println("Failure")
        sys.exit(1)
    } recoverWith { case e =>
      println("Error: " + e.getMessage)
      sys.exit(1)
    }
  }
}
