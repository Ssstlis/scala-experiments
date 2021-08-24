package old.lib

trait ReCompose[F[_, _], G[_, _]] {
  def preCompose[A, B, C, D]: G[C, D] => F[A, G[B, C]] => F[A, G[B, D]]

  def preAndThen[A, B, C, D]: G[B, C] => F[A, G[C, D]] => F[A, G[B, D]]

  def pairCompose[A, A1, B, C, D]: F[A, G[C, D]] => F[A1, G[B, C]] => F[(A1, A), G[B, D]]

  def pairAndThen[A, A1, B, C, D]: F[A1, G[B, C]] => F[A, G[C, D]] => F[(A1, A), G[B, D]] = f => g => {
    pairCompose(g)(f)
  }
}

object ReCompose {
  def apply[F[_, _], G[_, _]](implicit instance: ReCompose[F, G]): ReCompose[F, G] = instance

  implicit def toReComposeOps[F[_, _], G[_, _], A, X1, X2](f: F[A, G[X1, X2]])(implicit FG: ReCompose[F, G]): ToReComposeOps[F, G, A, X1, X2] = {
    new ToReComposeOps(f)
  }

  class ToReComposeOps[F[_, _], G[_, _], A, X1, X2](f: F[A, G[X1, X2]])(implicit FG: ReCompose[F, G])  {
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

