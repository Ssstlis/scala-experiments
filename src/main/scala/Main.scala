import scala.reflect.ClassTag

import ReComposeTest._
import cats.Id
import cats.data.Kleisli
import shapeless.ops.tuple.{Length, Prepend, Split}
import shapeless.{Nat, Succ, _0}
import shapeless.syntax.std.tuple._
import shapeless.syntax.nat._
import ReCompose._

object Main {
  def main(args: Array[String]): Unit = {

    val a: String => String => Int = f1 => f2 => (f1.toInt + 1) * (f2.toInt + 1)
    val b: String => Int => String = f1 => f2 => ((f1.toInt + 1) * f2).toString
    val c1: Long => String => Boolean = _ => _ => true
    val d1: Boolean => Boolean => Char = _ => _ => 'r'

    val c: String => Kleisli[Id, Int, String] = _ => Kleisli[Id, Int, String](_ => "123")
    val d: String => Kleisli[Id, String, Boolean] = _ => Kleisli[Id, String, Boolean](_ => true)
    val e: String => Kleisli[Id, Boolean, Long] = _ => Kleisli[Id, Boolean, Long](_ => 0L)
    val f: String => Kleisli[Id, Long, String] = _ => Kleisli[Id, Long, String](_ => "1234")

    class toReComposeOps1[A: ClassTag, B, C](f: A => B => C) {
      def pairAndThen[A1: ClassTag, D, L <: Nat, P](g: A1 => C => D)(
        implicit L1: Length.Aux[A, L], P: Prepend.Aux[A, A1, P], S: Split[P, L]
      ): P => B => D = {
        p => S(p) match {
          case (a: A, a1: A1) => f(a) andThen g(a1)
        }
      }
      def >++> : Int = 3
      def >+:> : Int = 3
      def >:+> : Int = 3
    }

    def toReCo[A: ClassTag, B, C](f: A => B => C): toReComposeOps1[A, B, C] = {
      new toReComposeOps1[A, B, C](f)
    }

    toReCo(a.pairAndThen(b)).pairAndThen(c1.compose((_: Tuple1[Long])._1))

    c.pairAndThen(d).pairAndThen(e).pairAndThen(f)(("", ("", ("", ""))))

    println(a.pairAndThen(b)("1", "2")("3"))

    println(c.pairAndThen(d)("1", "2")(0))
  }
}
