import scala.reflect.ClassTag

import cats.{Apply, Id, Monad}
import cats.data.Kleisli
import shapeless.ops.tuple.{Length, Prepend, Split}
import shapeless.{HNil, Lazy, Nat, Poly, Succ, _0}
import shapeless.syntax.std.tuple._
import shapeless.syntax.nat._
import ReCompose._
import cats.arrow.{Arrow, Compose}
import cats.effect.IO
import cats.implicits._
import shapeless.ops.tuple
import cats.syntax.arrow.toArrowOps
import cats.syntax.compose.toComposeOps

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

//    def testProduct[T <: Product](value: T): T = value
//
//    testProduct("sd,jfb")

    // A B C D
    // 0 0 0 1
    // 0 0 1 0
    // 0 0 1 1
    // 0 1 0 0
    // 0 1 1 0
    // 0 1 1 1
    // 1 0 0 0
    // 1 0 0 1
    // 1 0 1 0
    // 1 0 1 1
    // 1 1 0 0
    // 1 1 0 1
    // 1 1 1 0
    // 1 1 1 1


    class SplitArrowed[F[_, _]] {
      def apply[A, B <: Nat](arg: Split[A, B])(implicit F: Arrow[F]): F[A, arg.Out] = {
        F.lift(arg(_: A))
      }
    }

    class PrependArrowed[F[_, _]] {
      def apply[A, B](arg: Prepend[A, B])(implicit F: Arrow[F]): F[(A, B), arg.Out] = {
        F.lift { case (b, d) => arg(b, d) }
      }
    }

    def arrowedS[F[_, _]]: SplitArrowed[F] = new SplitArrowed[F]

    def arrowedP[F[_, _]]: PrependArrowed[F] = new PrependArrowed[F]

    def tupled[F[_, _]: Arrow, A]: F[A, Tuple1[A]] = Arrow[F].lift(Tuple1(_: A))

    def tupledR[F[_, _]: Arrow, A, B]: F[(A, B), (A, Tuple1[B])] = {
      tupled[F, B].second[B]
    }

    def tupledL[F[_, _]: Arrow, A, B]: F[(A, B), (Tuple1[A], B)] = {
      tupled[F, A].first[B]
    }

    implicit class toReComposeTupleOps2[A, B, F[_, _]: Arrow](f: F[A, B]) {
      def &++&[C](g: F[A, C])(implicit P: Prepend[B, C]): F[A, P.Out] = {
        (f &&& g) >>> arrowedP[F](P)
      }

      def &:+&[C](g: F[A, C])(implicit P: Prepend[B, Tuple1[C]]): F[A, P.Out] = {
        (f &&& (g >>> tupled[F, C])) >>> arrowedP[F](P)
      }

      def &+:&[C](g: F[A, C])(implicit P: Prepend[Tuple1[B], C]): F[A, P.Out] = {
        ((f >>> tupled[F, B]) &&& g) >>> arrowedP[F](P)
      }
    }


    implicit class toReComposeTupleOps3[A <: Product, B <: Product, F[_, _]: Arrow](f: F[A, B]) {
      //A, B, C, D - Product
      def *+++*[C <: Product, D <: Product, N1 <: Nat, T1](g: F[C, D])(
        implicit
        L1: Length.Aux[A, N1],
        P1: Prepend.Aux[A, C, T1],
        P2: Prepend[B, D],
        S1: Split.Aux[T1, N1, (A, C)]
      ): F[T1, P2.Out] = {
        arrowedP[F](P2) <<< (f *** g) <<< arrowedS[F](S1)
      }

      //A, B, C - Product
      def *+++*[C <: Product, D, N1 <: Nat, N2 <: Nat, T1, T2](g: F[C, D])(
        implicit
        L1: Length.Aux[A, N1],
        L2: Length.Aux[B, N2],
        P1: Prepend.Aux[A, C, T1],
        P2: Prepend.Aux[B, Tuple1[D], T2],
        S1: Split.Aux[T1, N1, (A, C)]
      ): F[T1, T2] = {
        arrowedS[F](S1) >>> (f *** g) >>> tupledR[F, B, D] >>> arrowedP[F](P2)
      }

      //A, B, D - Product
      def *+++*[C, D <: Product, N1 <: Nat, N2 <: Nat, T1, T2](g: F[C, D])(
        implicit
        L1: Length.Aux[A, N1],
        L2: Length.Aux[B, N2],
        P1: Prepend.Aux[A, Tuple1[C], T1],
        P2: Prepend.Aux[B, D, T2],
        S1: Split.Aux[T1, N1, (A, C)]
      ): F[T1, T2] = {
        (f *** g) <<< tupledR[F, A, C]
        arrowedS[F](S1) >>> (f *** g) >>> tupledR[F, A, C] >>> arrowedP[F](P2)
      }

      //A, B, C, D, - Product
      /*def *+:+*[C, D, N1 <: Nat, T1](g: F[C, D])(
        implicit
        L1: Length.Aux[A, N1],
        P1: Prepend.Aux[A, C, T1],
        P2: Prepend[B, D],
        S1: Split.Aux[T1, N1, (A, C)]
      ): F[T1, P2.Out] = {
        ((f *** g) <<< arrowed[F](S1)) >>> Arrow[F].lift[(B, D), P2.Out] { case (b, d) => P2(b, d) }
      }*/

//      def &:+&[C](g: F[A, C])(implicit P: Prepend[B, Tuple1[C]]): F[A, P.Out] = {
//        (f &&& (g >>> Arrow[F].lift(Tuple1(_: C)))) >>> Arrow[F].lift[(B, Tuple1[C]), P.Out] { case (b, c) => P(b, c) }
//      }
//
//      def &+:&[C](g: F[A, C])(implicit P: Prepend[Tuple1[B], C]): F[A, P.Out] = {
//        ((f >>> Arrow[F].lift(Tuple1(_: B))) &&& g) >>> Arrow[F].lift[(Tuple1[B], C), P.Out] { case (b, c) => P(b, c) }
//      }
    }

    // A A1
    // 0 1
    // 1 0
    // 1 1

    implicit class toReComposeTupleProductOps[A <: Product, B, C, F[_, _]: Arrow, G[_, _]](f: F[A, G[B, C]])(implicit RG: ReCompose[F, G]) {

      def >++>[A1 <: Product, D, L <: Nat, P](g: F[A1, G[C, D]])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, A1, P],
        S: Split.Aux[P, L, (A, A1)]
      ): F[P, G[B, D]] = (g <**< f) <<< arrowedS[F](S)

      def >:+>[A1, D, L <: Nat, P](g: F[A1, G[C, D]])(
        implicit
        L1: Length.Aux[A, L],
        P: Prepend.Aux[A, Tuple1[A1], P],
        S: Split.Aux[P, L, (A, A1)]
      ): F[P, G[B, D]] = (g <**< f) <<< arrowedS[F](S)

      def <++<[A1 <: Product, D, L <: Nat, P](g: F[A1, G[D, B]])(
        implicit
        L1: Length.Aux[A1, L],
        P: Prepend.Aux[A1, A, P],
        S: Split.Aux[P, L, (A1, A)]
      ): F[P, G[D, C]] = (f <**< g) <<< arrowedS[F](S)

      def <:+<[A1, D, P](g: F[A1, G[D, B]])(
        implicit
        P: Prepend.Aux[Tuple1[A1], A, P],
        S: Split.Aux[P, Nat._1, (A1, A)]
      ): F[P, G[D, C]] = (f <**< g) <<< arrowedS[F](S)
    }

    implicit class toReComposeTupleOps[A, B, C, F[_, _]: Arrow, G[_, _]](f: F[A, G[B, C]])(implicit RG: ReCompose[F, G]) {

      def >+:>[A1 <: Product, D, P](g: F[A1, G[C, D]])(
        implicit
        P: Prepend.Aux[Tuple1[A], A1, P],
        S: Split.Aux[P, Nat._1, (A, A1)]
      ): F[P, G[B, D]] = (g <**< f) <<< arrowedS[F](S)

      def <+:<[A1 <: Product, D, L <: Nat, P](g: F[A1, G[D, B]])(
        implicit
        L1: Length.Aux[A1, L],
        P: Prepend.Aux[A1, Tuple1[A], P],
        S: Split.Aux[P, L, (A1, A)]
      ): F[P, G[D, C]] = (f <**< g) <<< arrowedS[F](S)

    }

    val testing0: ((String, String, Long, Boolean)) => String => Char = (b <**< a) >++> (d1 <**< c1)

    val testing1: ((String, String, Long, Boolean)) => String => Char = (d1 <**< c1) <++< (b <**< a)

    val testing2: ((String, String, String, String)) => Kleisli[Id, Int, String] = (d <**< c) >++> (f <**< e)

    val testing3: ((String, String, String, String)) => Kleisli[Id, Int, String] = (f <**< e) <++< (d <**< c)

    ()
  }


}
