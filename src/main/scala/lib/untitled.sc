//import cats.Id
//import scala.language.experimental.macros
//import scala.reflect.macros.blackbox.Context
//import scala.util.matching.Regex
//
//val regex: Regex = "^([\\w]+\\.)*[\\w]+$".r
//def helloWorld(): String = macro helloWorld_impl
//
//def helloWorld_impl(c: Context)(s: c.Expr[String]): c.Expr[String] = {
//  import c.universe._
//
//  s.value
//
//  c.abort()
//}

def tailrec[A, B](start: A)(f: A => Either[B, A]): B = {

  @scala.annotation.tailrec
  def loop(state: Either[B, A]): B = {
    state match {
      case Left(left) => left
      case Right(right) => loop(f(right))
    }
  }

  loop(Right(start))
}

tailrec((1, 5)) { case (acc, v) => if (v > 0) Right((acc * v, v -1)) else Left(acc) }