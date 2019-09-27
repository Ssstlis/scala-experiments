import ReComposeTest._
import ReComposeTest.ReCompose._

object Main {
  def main(args: Array[String]): Unit = {

    println(a.pairAndThen(b)("1", "2")("3"))

    println(c.pairAndThen(d)("1", "2")(0))
  }
}
