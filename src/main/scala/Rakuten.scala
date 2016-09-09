import client.Remambo
import client.Service.actorSystem

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Rakuten {
  def main(args: Array[String]): Unit = {

    val (username, password, url) = (args(0), args(1), args(2))

    val remambo = Remambo(username, password)
    remambo.buyRakuten(url) map {
      case Success(true) =>
        println("Success")
        sys.exit(0)
      case Success(false) =>
        println("Failure")
        sys.exit(1)
      case Failure(e) =>
        println("Error: " + e.getMessage)
        sys.exit(1)
    }
  }
}
