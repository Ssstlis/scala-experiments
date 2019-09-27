
import cats.arrow.ArrowChoice
import cats.{FlatMap, Id}
import cats.data.Kleisli
import cats.implicits._

object ReComposeTest {

  ArrowChoice[Function1]

  ((_: String) => 3).merge(_ => true)

  implicit class FirstApproxReCompose[A, B, C](run: A => B => C) {
    def >*>[A1, B1, C1](f: A1 => B1 => C1): (A, A1) => (B => C, B1 => C1) = {
      case (a, a1) => (run(a), f(a1))
    }

    def >**>[A1, D](f: A1 => C => D): (A, A1) => B => D = {
      case (a, a1) => f(a1) compose run(a)
    }

    def <**<[A1, D](f: A1 => D => B): (A, A1) => D => C = {
      case (a, a1) => run(a) compose f(a1)
    }
  }

  trait ReCompose {
    type F[_, _]

    type G[_,_]

    def pairCompose[A, A1, B, C, D](f: F[A, G[C, D]])(g: F[A1, G[B, C]]): F[(A, A1), G[B, D]]

    def pairAndThen[A, A1, B, C, D](f: F[A1, G[B, C]])(g: F[A, G[C, D]]): F[(A, A1), G[B, D]] = {
      pairCompose[A, A1, B, C, D](g)(f)
    }


    def <**<[A, A1, B, C, D](f: F[A, G[C, D]])(g: F[A1, G[B, C]]): F[(A, A1), G[B, D]] = {
      pairCompose(f)(g)
    }

    def >**>[A, A1, B, C, D](f: F[A1, G[B, C]])(g: F[A, G[C, D]]): F[(A, A1), G[B, D]] = {
      pairAndThen(f)(g)
    }
  }

  object ReCompose {
    type Aux[F1[_, _], G1[_, _]] = ReCompose {
      type F[X, X1] = F1[X, X1]
      type G[X, X1] = G1[X, X1]
    }

    def apply[F[_, _], G[_, _]](implicit instance: ReCompose.Aux[F, G]): Aux[F, G] = instance

    implicit class toMyComposeOps[F[_, _], G[_, _], A, X1, X2](f: F[A, G[X1, X2]])(implicit instance: ReCompose.Aux[F, G])  {
      def pairAndThen[A1, X3](g: F[A1, G[X2, X3]]): F[(A1, A), G[X1, X3]] = instance.pairAndThen(f)(g)
    }
  }

  implicit val reComposeFuncFuncInstance: ReCompose.Aux[Function1, Function1] = new ReCompose {
    type F[X, X1] = X => X1
    type G[X, X1] = X => X1

    def pairCompose[A, A1, B, C, D](f: A => C => D)(g: A1 => B => C): ((A, A1)) => B => D = { case (a, a1) =>
      f(a) compose g(a1)
    }
  }

  implicit def reComposeFuncKleisliInstance[Y[_]: FlatMap]: ReCompose {
    type F[X, X1] = X => X1

    type G[X, X1] = Kleisli[Y, X, X1]
  } = new ReCompose {
    type F[X, X1] = X => X1
    type G[X, X1] = Kleisli[Y, X, X1]

    def pairCompose[A, A1, B, C, D](f: A => Kleisli[Y, C, D])(g: A1 => Kleisli[Y, B, C]): ((A, A1)) => Kleisli[Y, B, D] = {
      case (a, a1) => f(a).compose(g(a1))
    }
  }

  implicit def reComposeKleisliKleisliInstance[Y[_]](implicit Y: FlatMap[Y]): ReCompose {
    type G[X, X1] = Kleisli[Y, X, X1]

    type F[X, X1] = Kleisli[Y, X, X1]
  } = new ReCompose {
    type G[X, X1] = Kleisli[Y, X, X1]

    type F[X, X1] = Kleisli[Y, X, X1]

    def pairCompose[A, A1, B, C, D](f: Kleisli[Y, A, Kleisli[Y, C, D]])(g: Kleisli[Y, A1, Kleisli[Y, B, C]]): Kleisli[Y, (A, A1), Kleisli[Y, B, D]] = {
      Kleisli { case (a, a1) => Y.flatMap(f.run(a))(f1 => Y.map(g.run(a1))(f1.compose(_))) }
    }
  }

  val a: String => String => Int = f1 => f2 => (f1.toInt + 1) * (f2.toInt + 1)
  val b: String => Int => String = f1 => f2 => ((f1.toInt + 1) * f2).toString

  val c: String => Kleisli[Id, Int, String] = _ => Kleisli[Id, Int, String](_ => "123")
  val d: String => Kleisli[Id, String, Boolean] = _ => Kleisli[Id, String, Boolean](_ => true)
  import ReCompose._

  a.pairAndThen(b)

  c.pairAndThen(d)
}