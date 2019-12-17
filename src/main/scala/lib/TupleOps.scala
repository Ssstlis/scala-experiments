package lib

import cats.arrow.Arrow
import lib.Helpers.{<~-~, tupled, tupledR, ~-~>}
import shapeless.Nat
import shapeless.ops.tuple.{Length, Prepend, Split}
import cats.implicits.toArrowOps
import cats.implicits.toComposeOps
import lib.ReCompose.toReComposeOps

object TupleOps {
  implicit class toReComposeTupleOps2[A, B <: Product, F[_, _]: Arrow](f: F[A, B]) {
    def &++&[C <: Product](g: F[A, C])(implicit P: Prepend[B, C]): F[A, P.Out] = {
      (f &&& g) >>> <~-~[F](P)
    }

    def &:+&[C](g: F[A, C])(implicit P: Prepend[B, Tuple1[C]]): F[A, P.Out] = {
      (f &&& (g >>> tupled[F, C])) >>> <~-~[F](P)
    }

    def &+:&[C <: Product](g: F[A, C])(implicit P: Prepend[Tuple1[B], C]): F[A, P.Out] = {
      ((f >>> tupled[F, B]) &&& g) >>> <~-~[F](P)
    }
  }


  implicit class toReComposeTupleOps3[A <: Product, B <: Product, F[_, _]: Arrow](f: F[A, B]) {
    //A, B, C, D - Product
    def *++++*[C <: Product, D <: Product, N1 <: Nat, T1](g: F[C, D])(
      implicit
      L1: Length.Aux[A, N1],
      P1: Prepend.Aux[A, C, T1],
      P2: Prepend[B, D],
      S1: Split.Aux[T1, N1, (A, C)]
    ): F[T1, P2.Out] = {
      <~-~[F](P2) <<< (f *** g) <<< ~-~>[F](S1)
    }

    //A, B, C - Product
    def *+++|*[C <: Product, D, N1 <: Nat, N2 <: Nat, T1, T2](g: F[C, D])(
      implicit
      L1: Length.Aux[A, N1],
      L2: Length.Aux[B, N2],
      P1: Prepend.Aux[A, C, T1],
      P2: Prepend.Aux[B, Tuple1[D], T2],
      S1: Split.Aux[T1, N1, (A, C)]
    ): F[T1, T2] = {
      ~-~>[F](S1) >>> (f *** g) >>> tupledR[F, B, D] >>> <~-~[F](P2)
    }

    //A, B, D - Product
    def *++|+*[C, D <: Product, N1 <: Nat, N2 <: Nat, T1, T2](g: F[C, D])(
      implicit
      L1: Length.Aux[A, N1],
      L2: Length.Aux[B, N2],
      P1: Prepend.Aux[A, Tuple1[C], T1],
      P2: Prepend.Aux[B, D, T2],
      S1: Split.Aux[T1, N1, (A, C)],
    ): F[T1, T2] = {
      <~-~[F](P2) <<< (f *** g) <<< ~-~>[F](S1)
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
    ): F[P, G[B, D]] = (g <**< f) <<< ~-~>[F](S)

    def >:+>[A1, D, L <: Nat, P](g: F[A1, G[C, D]])(
      implicit
      L1: Length.Aux[A, L],
      P: Prepend.Aux[A, Tuple1[A1], P],
      S: Split.Aux[P, L, (A, A1)]
    ): F[P, G[B, D]] = (g <**< f) <<< ~-~>[F](S)

    def <++<[A1 <: Product, D, L <: Nat, P](g: F[A1, G[D, B]])(
      implicit
      L1: Length.Aux[A1, L],
      P: Prepend.Aux[A1, A, P],
      S: Split.Aux[P, L, (A1, A)]
    ): F[P, G[D, C]] = (f <**< g) <<< ~-~>[F](S)

    def <:+<[A1, D, P](g: F[A1, G[D, B]])(
      implicit
      P: Prepend.Aux[Tuple1[A1], A, P],
      S: Split.Aux[P, Nat._1, (A1, A)]
    ): F[P, G[D, C]] = (f <**< g) <<< ~-~>[F](S)
  }

  implicit class toReComposeTupleOps[A, B, C, F[_, _]: Arrow, G[_, _]](f: F[A, G[B, C]])(implicit RG: ReCompose[F, G]) {

    def >+:>[A1 <: Product, D, P](g: F[A1, G[C, D]])(
      implicit
      P: Prepend.Aux[Tuple1[A], A1, P],
      S: Split.Aux[P, Nat._1, (A, A1)]
    ): F[P, G[B, D]] = (g <**< f) <<< ~-~>[F](S)

    def <+:<[A1 <: Product, D, L <: Nat, P](g: F[A1, G[D, B]])(
      implicit
      L1: Length.Aux[A1, L],
      P: Prepend.Aux[A1, Tuple1[A], P],
      S: Split.Aux[P, L, (A1, A)]
    ): F[P, G[D, C]] = (f <**< g) <<< ~-~>[F](S)

  }
}
