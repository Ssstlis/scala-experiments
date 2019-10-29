
import cats.arrow.Compose
import cats.data.{Cokleisli, Kleisli}
import cats.{Apply, Bimonad}
import cats.syntax.apply.catsSyntaxApply
import cats.syntax.compose.toComposeOps
import cats.syntax.comonad.toComonadOps
import cats.syntax.applicative.catsSyntaxApplicativeId


trait ReCompose[F[_, _], G[_, _]] {
  def preCompose[A, B, C, D]: G[C, D] => F[A, G[B, C]] => F[A, G[B, D]]

  def preAndThen[A, B, C, D]: G[B, C] => F[A, G[C, D]] => F[A, G[B, D]]

  def pairCompose[A, A1, B, C, D]: F[A, G[C, D]] => F[A1, G[B, C]] => F[(A1, A), G[B, D]]

  def pairAndThen[A, A1, B, C, D]: F[A1, G[B, C]] => F[A, G[C, D]] => F[(A1, A), G[B, D]] = f => g => {
    pairCompose(g)(f)
  }
}

object ReCompose extends Instances {
  def apply[F[_, _], G[_, _]](implicit instance: ReCompose[F, G]): ReCompose[F, G] = instance

  implicit class toReComposeOps[F[_, _], G[_, _], A, X1, X2](f: F[A, G[X1, X2]])(implicit FG: ReCompose[F, G])  {
    def pairAndThen[A1, X3](g: F[A1, G[X2, X3]]): F[(A, A1), G[X1, X3]] = FG.pairAndThen(f)(g)

    /**
     *     Alias for [[pairAndThen]]
     */
    def >**>[A1, X3](g: F[A1, G[X2, X3]]): F[(A, A1), G[X1, X3]] = FG.pairAndThen(f)(g)

    def pairCompose[A1, X3](g: F[A1, G[X3, X1]]): F[(A1, A), G[X3, X2]] = FG.pairCompose(f)(g)

    /**
     * Alias for [[pairCompose]]
     */
    def <**<[A1, X3](g: F[A1, G[X3, X1]]): F[(A1, A), G[X3, X2]] = FG.pairCompose(f)(g)

    def preCompose[X3](g: G[X2, X3]): F[A, G[X1, X3]] = FG.preCompose(g)(f)

    /**
     * Alias for [[preCompose]]
     */
    def <*<[X3](g: G[X2, X3]): F[A, G[X1, X3]] = FG.preCompose(g)(f)

    def preAndThen[X3](g: G[X3, X1]): F[A, G[X3, X2]] = FG.preAndThen(g)(f)

    /**
     * Alias for [[preAndThen]]
     */
    def >*>[X3](g: G[X3, X1]): F[A, G[X3, X2]] = FG.preAndThen(g)(f)
  }
}

trait Instances {

  implicit def stdFunction1ReComposeInstance[F[_, _]: Compose]: ReCompose[? => ?, F] = new ReCompose[? => ?, F] {
    def preCompose[A, B, C, D] = g => f => g <<< f(_)

    def preAndThen[A, B, C, D] = g => f => f(_) <<< g

    def pairCompose[A, A1, B, C, D] = g => f => { case (a1, a) => g(a) <<< f(a1) }
  }

  implicit def stdKleislireComposeInstance[F[_]: Apply, G[_, _]: Compose]: ReCompose[Kleisli[F, ?, ?], G] = {
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