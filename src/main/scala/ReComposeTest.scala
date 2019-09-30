
import cats.FlatMap
import cats.data.Kleisli

object ReComposeTest {
  type Kl[F[_], A, B] = Kleisli[F, A, B]

  trait ReCompose[F[_, _], G[_, _]] {
    def preCompose[A, B, C, D](f: G[B, C])(g: F[A, G[C, D]]): F[A, G[B, D]]

    def preAndThen[A, B, C, D](f: F[A, G[B, C]])(g: G[C, D]): F[A, G[B, D]]

    def pairCompose[A, A1, B, C, D](f: F[A, G[C, D]])(g: F[A1, G[B, C]]): F[(A, A1), G[B, D]]

    def pairAndThen[A, A1, B, C, D](f: F[A1, G[B, C]])(g: F[A, G[C, D]]): F[(A, A1), G[B, D]] = {
      pairCompose[A, A1, B, C, D](g)(f)
    }

  }

  object ReCompose {
    def apply[F[_, _], G[_, _]](implicit instance: ReCompose[F, G]): ReCompose[F, G] = instance

    implicit class toReComposeOps[F[_, _], G[_, _], A, X1, X2](f: F[A, G[X1, X2]])(implicit instance: ReCompose[F, G])  {
      def pairAndThen[A1, X3](g: F[A1, G[X2, X3]]): F[(A1, A), G[X1, X3]] = instance.pairAndThen(f)(g)

      def >**>[A1, X3](g: F[A1, G[X2, X3]]): F[(A1, A), G[X1, X3]] = instance.pairAndThen(f)(g)

      def pairCompose[A1, X3](g: F[A1, G[X3, X1]]): F[(A, A1), G[X3, X2]] = instance.pairCompose(f)(g)

      def <**<[A1, X3](g: F[A1, G[X3, X1]]): F[(A, A1), G[X3, X2]] = instance.pairCompose(f)(g)

      def preCompose[X3](g: G[X3, X1]): F[A, G[X3, X2]] = instance.preCompose(g)(f)

      def <*<[X3](g: G[X3, X1]): F[A, G[X3, X2]] = instance.preCompose(g)(f)

      def preAndThen[X3](g: G[X2, X3]): F[A, G[X1, X3]] = instance.preAndThen(f)(g)

      def >*>[X3](g: G[X2, X3]): F[A, G[X1, X3]] = instance.preAndThen(f)(g)
    }
  }

  implicit val reComposeFuncFuncInstance: ReCompose[Function1, Function1] = {
    new ReCompose[Function1, Function1] {
      def pairCompose[A, A1, B, C, D](f: A => C => D)(g: A1 => B => C): ((A, A1)) => B => D = { case (a, a1) =>
        f(a) compose g(a1)
      }

      def preCompose[A, B, C, D](f: B => C)(g: A => C => D): A => B => D = { a =>
        g(a) compose f
      }

      def preAndThen[A, B, C, D](f: A => B => C)(g: C => D): A => B => D = { a =>
        g compose f(a)
      }
    }
  }

  implicit def reComposeFuncKleisliInstance[Y[_]: FlatMap]: ReCompose[Function1, Kl[Y, ?, ?]] = {
    new ReCompose[Function1, Kl[Y, ?, ?]] {
      def pairCompose[A, A1, B, C, D](f: A => Kl[Y, C, D])(g: A1 => Kl[Y, B, C]): ((A, A1)) => Kl[Y, B, D] = {
        case (a, a1) => f(a).compose(g(a1))
      }

      def preCompose[A, B, C, D](f: Kl[Y, B, C])(g: A => Kl[Y, C, D]): A => Kl[Y, B, D] = { a =>
        g(a) compose f
      }

      def preAndThen[A, B, C, D](f: A => Kl[Y, B, C])(g: Kl[Y, C, D]): A => Kl[Y, B, D] = { a =>
        g compose f(a)
      }
    }
  }

  implicit def reComposeKleisliFuncInstance[Y[_]](implicit Y: FlatMap[Y]): ReCompose[Kl[Y, ?, ?], Function1] = {
    new ReCompose[Kl[Y, ?, ?], Function1] {
      def preCompose[A, B, C, D](f: B => C)(g: Kl[Y, A, C => D]): Kl[Y, A, B => D] = {
        Kleisli(a => Y.map(g.run(a))(_ compose f))
      }

      def preAndThen[A, B, C, D](f: Kl[Y, A, B => C])(g: C => D): Kl[Y, A, B => D] = {
        Kleisli(a => Y.map(f.run(a))(g compose _))
      }

      def pairCompose[A, A1, B, C, D](f: Kl[Y, A, C => D])(g: Kl[Y, A1, B => C]): Kl[Y, (A, A1), B => D] = {
        Kleisli { case (a, a1) => Y.flatMap(g.run(a1))(g1 => Y.map(f.run(a))(_ compose g1))}
      }
    }
  }

  implicit def reComposeKleisliKleisliInstance1[Y[_]](implicit Y: FlatMap[Y]): ReCompose[Kl[Y, ?, ?], Kl[Y, ?, ?]] = {
    new ReCompose[Kl[Y, ?, ?], Kl[Y, ?, ?]] {
      def pairCompose[A, A1, B, C, D](f: Kl[Y, A, Kl[Y, C, D]])(g: Kl[Y, A1, Kl[Y, B, C]]): Kl[Y, (A, A1), Kl[Y, B, D]] = {
        Kleisli { case (a, a1) => Y.flatMap(g.run(a1))(g1 => Y.map(f.run(a))(_ compose g1))}
      }

      override def preCompose[A, B, C, D](f: Kl[Y, B, C])(g: Kl[Y, A, Kl[Y, C, D]]): Kl[Y, A, Kl[Y, B, D]] = {
        Kleisli(a => Y.map(g.run(a))(_ compose f))
      }

      override def preAndThen[A, B, C, D](f: Kl[Y, A, Kl[Y, B, C]])(g: Kl[Y, C, D]): Kl[Y, A, Kl[Y, B, D]] = {
        Kleisli(a => Y.map(f.run(a))(g compose _))
      }
    }
  }

}