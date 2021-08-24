package old.lib

import cats.arrow.Arrow
import cats.syntax.strong.toStrongOps
import shapeless.ops.tuple.{Prepend, Split}
import shapeless.{::, HNil, Nat}

object Helpers {

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

  def ~-~>[F[_, _]] = new {
    def apply[A, B <: Nat](arg: Split[A, B])(implicit F: Arrow[F]): F[A, arg.Out] = {
      F.lift(arg(_: A))
    }
  }

  def <~-~[F[_, _]] = new {
    def apply[A, B](arg: Prepend[A, B])(implicit F: Arrow[F]): F[(A, B), arg.Out] = {
      F.lift { case (b, d) => arg(b, d) }
    }
  }


  def tupled[F[_, _]: Arrow, A]: F[A, Tuple1[A]] = Arrow[F].lift(Tuple1(_: A))

  def untupled[F[_, _]: Arrow, A]: F[Tuple1[A], A] = Arrow[F].lift(_._1)


  def listed[F[_, _]: Arrow, A]: F[A, A :: HNil] = Arrow[F].lift(_ :: HNil)

  def unlisted[F[_, _]: Arrow, A]: F[A :: HNil, A] = Arrow[F].lift(_.head)


  def tupledR[F[_, _]: Arrow, A, B]: F[(A, B), (A, Tuple1[B])] = tupled[F, B].second[A]

  def tupledL[F[_, _]: Arrow, A, B]: F[(A, B), (Tuple1[A], B)] = tupled[F, A].first[B]

  def untupledR[F[_, _]: Arrow, A, B]: F[(A, Tuple1[B]), (A, B)] = untupled[F, B].second[A]

  def untupledL[F[_, _]: Arrow, A, B]: F[(Tuple1[A], B), (A, B)] = untupled[F, A].first[B]


  def listedR[F[_, _]: Arrow, A, B]: F[(A, B), (A, B :: HNil)] = listed[F, B].second[A]

  def listedL[F[_, _]: Arrow, A, B]: F[(A, B), (A :: HNil, B)] = listed[F, A].first[B]

  def unlistedR[F[_, _]: Arrow, A, B]: F[(A, B :: HNil), (A, B)] = unlisted[F, B].second[A]

  def unlistedL[F[_, _]: Arrow, A, B]: F[(A :: HNil, B), (A, B)] = unlisted[F, A].first[B]
}
