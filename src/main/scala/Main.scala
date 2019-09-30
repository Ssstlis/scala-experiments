import ReComposeTest._
import cats.Id
import cats.data.Kleisli

object Main {
  def main(args: Array[String]): Unit = {

    val a: String => String => Int = f1 => f2 => (f1.toInt + 1) * (f2.toInt + 1)
    val b: String => Int => String = f1 => f2 => ((f1.toInt + 1) * f2).toString

    val c: String => Kleisli[Id, Int, String] = _ => Kleisli[Id, Int, String](_ => "123")
    val d: String => Kleisli[Id, String, Boolean] = _ => Kleisli[Id, String, Boolean](_ => true)
    val e: String => Kleisli[Id, Boolean, Long] = _ => Kleisli[Id, Boolean, Long](_ => 0L)
    val f: String => Kleisli[Id, Long, String] = _ => Kleisli[Id, Long, String](_ => "1234")
    import ReCompose._

    a.pairAndThen(b)

    c.pairAndThen(d).pairAndThen(e).pairAndThen(f)(("", ("", ("", ""))))

    println(a.pairAndThen(b)("1", "2")("3"))

    println(c.pairAndThen(d)("1", "2")(0))
  }
}
