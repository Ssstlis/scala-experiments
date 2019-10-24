import scala.reflect.ClassTag

import ReComposeTest._
import cats.{Id, Monad}
import cats.data.Kleisli
import shapeless.ops.tuple.{Length, Prepend, Split}
import shapeless.{Lazy, Nat, Succ, _0}
import shapeless.syntax.std.tuple._
import shapeless.syntax.nat._
import ReCompose._
import cats.arrow.Compose
import cats.implicits._

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

    trait Getter[F[_, _]] {
      type G[_]
      def get[A, B](f: F[A, G[B]])(a: A): G[B]
    }


    implicit class toReComposeOps1[A: ClassTag, B, C, F[_, _]: Compose](f: A => F[B, C]) {

      def >++>[A1: ClassTag, D, L <: Nat, P](g: A1 => F[C, D])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, A1, P],
        S: Split[P, L]
      ): P => F[B, D] = p => S(p) match {
        case (a: A, a1: A1) => Compose[F].compose(g(a1), f(a))
      }

      def >:+>[A1: ClassTag, D, L <: Nat, P](g: A1 => F[C, D])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, Tuple1[A1], P],
        S: Split[P, L]
      ): P => F[B, D] = p => S(p) match {
        case (a: A, a1: A1) => Compose[F].compose(g(a1), f(a))
      }

      def >+:>[A1: ClassTag, D, L <: Nat, P](g: A1 => F[C, D])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[Tuple1[A], A1, P],
        S: Split[P, L]
      ): P => F[B, D] = p => S(p) match {
        case (a: A, a1: A1) => Compose[F].compose(g(a1), f(a))
      }
    }

    implicit class toReComposeOps2[A: ClassTag, B, C, F[_, _]: Compose, G[_]: Monad](f: Kleisli[G, A, F[B, C]]) {

      def >++>[A1: ClassTag, D, L <: Nat, P](g: Kleisli[G, A1, F[C, D]])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, A1, P],
        S: Split[P, L]
      ): Kleisli[G, P, F[B, D]] = Kleisli(p => S(p) match {
        case (a: A, a1: A1) => Monad[G].map2(g(a1), f(a))(Compose[F].compose)
      })

      def >:+>[A1: ClassTag, D, L <: Nat, P](g: Kleisli[G, A1, F[C, D]])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, Tuple1[A1], P],
        S: Split[P, L]
      ): Kleisli[G, P, F[B, D]] = Kleisli(p => S(p) match {
        case (a: A, a1: A1) => Monad[G].map2(g(a1), f(a))(Compose[F].compose)
      })

      def >+:>[A1: ClassTag, D, L <: Nat, P](g: Kleisli[G, A1, F[C, D]])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[Tuple1[A], A1, P],
        S: Split[P, L]
      ): Kleisli[G, P, F[B, D]] = Kleisli(p => S(p) match {
        case (a: A, a1: A1) => Monad[G].map2(g(a1), f(a))(Compose[F].compose)
      })
    }

    val testing = (a >**> b) >++> (c1 >**> d1)


    ()
  }


}
