package lib

import cats.{Apply, Bimonad}
import cats.arrow.Compose
import cats.data.{Cokleisli, Kleisli}
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.apply.catsSyntaxApply
import cats.syntax.comonad.toComonadOps
import cats.syntax.compose.toComposeOps

trait Instances {

  implicit def stdFunction1ReComposeInstance[F[_, _]: Compose]: ReCompose[? => ?, F] = new ReCompose[? => ?, F] {
    def preCompose[A, B, C, D] = g => f => g <<< f(_)

    def preAndThen[A, B, C, D] = g => f => f(_) <<< g

    def pairCompose[A, A1, B, C, D] = g => f => { case (a1, a) => g(a) <<< f(a1) }
  }

  implicit def stdKleisliReComposeInstance[F[_]: Apply, G[_, _]: Compose]: ReCompose[Kleisli[F, ?, ?], G] = {
    new ReCompose[Kleisli[F, ?, ?], G] {

      def preCompose[A, B, C, D] = g => _.map(g <<< _)

      def preAndThen[A, B, C, D] = g => _.map(_ <<< g)

      def pairCompose[A, A1, B, C, D] = g => f => {
        Kleisli { case (a1, a) => g(a).map2(f(a1))(_ <<< _) }
      }
    }
  }

  implicit def stdCokleisliReComposeInstance[F[_]: Bimonad, G[_, _]: Compose]: ReCompose[Cokleisli[F, ?, ?], G] = {
    new ReCompose[Cokleisli[F, ?, ?], G] {
      def preCompose[A, B, C, D] = g => _.map(g <<< _)

      def preAndThen[A, B, C, D] = g => _.map(_ <<< g)

      def pairCompose[A, A1, B, C, D] = g => f => Cokleisli(_.extract match {
        case (a1, a) => g.run(a.pure[F]) <<< f.run(a1.pure[F])
      })
    }
  }

}

object Instances extends Instances