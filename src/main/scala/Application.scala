import client.Service

object Application {
  def main(args: Array[String]): Unit = {

    val remambo = Service("Remambo", args(0), args(1))

    remambo.snipe(args(2), args(3).toShort)
  }
}
