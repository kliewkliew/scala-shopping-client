import client.{Credentials, Service}

object Application {
  def main(args: Array[String]): Unit = {

    val remambo = Service("Remambo")
    implicit val credentials = Credentials(args(0), args(1))

    remambo.bid(args(2), args(3).toShort)
  }
}
