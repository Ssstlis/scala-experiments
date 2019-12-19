import scala.concurrent.{ExecutionContext, Future}

import Main.ReFunc.Aux
import cats.arrow.{Arrow, Compose, Profunctor}
import cats.{Id, Monad, MonadError}
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import lib.Instances._
import lib.ReCompose.toReComposeOps
import lib.TupleOps._
import shapeless.{::, HNil, _}
import shapeless.HList.ListCompat._

object Main {
  type #:[+H, +T <: HList] = shapeless.::[H, T]

/*  sealed trait List[+A] extends Product with Serializable {
    def foldLeft[B](d: => B)(f: (A, B) => B): B = {
      @scala.annotation.tailrec
      def loop(acc: => B, rest: List[A]): B = {
        rest match {
          case Nil => acc
          case Cons(head, Nil) => f(head, acc)
          case Cons(head, tail) => loop(f(head, acc), tail)
        }
      }

      loop(d, this)
    }

    def +:[B <: A](elem: B): List[A] = Cons(elem, this)

    def foldRight[B](d: => B)(f: (A, B) => B): B = {
      this match {
        case Nil => d
        case Cons(head, Nil) => f(head, d)
        case Cons(head, tail) =>
      }
    }
  }

  final case object Nil extends List[Nothing]
  final case class Cons[A](head: A, tail: List[A]) extends List[A]*/

  trait Headed[T <: HList, U] extends DepFn1[T] with Serializable

  object Headed {
    def apply[T <: HList, U](implicit headed: Headed[T, U]): Headed[T, U] = headed

    type Aux[T <: HList, U, Out0] = Headed[T, U] { type Out = Out0 }

    implicit def fix[T <: HList, U]: Aux[U :: T, U, U :: T] = {
      new Headed[U #: T, U] {
        type Out = U #: T
        def apply(t: U #: T): U #: T = t
      }
    }

    implicit def recurse[T <: HList, U, H, L <: HList](
      implicit hd: Aux[T, U, U :: L]
    ): Aux[H :: T, U, U :: H :: L] = {
      new Headed[H :: T, U] {
        type Out = U :: H :: L

        def apply(t: H :: T): U :: H :: L = {
          t match {
            case h :: t => hd(t) match {
              case u #: l => #:(u, #:(h, l))
            }
          }
        }
      }
    }
  }


  def getHeaded[T <: HList, H](arg: T)(implicit headed: Headed[T, H]): headed.Out = headed(arg)

  getHeaded[Boolean :: Int :: Long :: HNil, Boolean](null)
  getHeaded[Int :: Boolean :: Long :: HNil, Boolean](null)

  trait Func[T] extends DepFn1[T] with Serializable

  object Func {
    def apply[T](implicit func: Func[T]): Aux[T, func.Out] = func

    type Aux[T, Out0] = Func[T] { type Out = Out0 }

    implicit def fix[A, B]: Aux[A :: HNil => B, A => B] = {
      new Func[A :: HNil => B] {
        type Out = A => B

        def apply(t: (A :: HNil) => B): Out = a => t(a :: HNil)
      }
    }

    implicit def recurse[H, R <: HList, B](
      implicit fn: Func[R => B]
    ): Aux[H :: R => B, H => fn.Out] = {
      new Func[H :: R => B] {
        type Out = H => fn.Out

        def apply(f: H :: R => B): Out = h => {
          fn(r => f(h :: r))
        }
      }
    }
  }

  trait ReFresh[F[_, _]] {

    val F: Profunctor[F]

    def combineP[A, B, C](f: F[A, F[B, C]]): F[(A, B), C]

    def combineH[A, B <: HList, C](f: F[A, F[B, C]]): F[A :: B, C] = {
      F.lmap(combineP(f)) { case a :: b => (a, b) }
    }

    def splitP[A, B, C](f: F[(A, B), C]): F[A, F[B, C]]

    def splitH[A, B <: HList, C](f: F[A :: B, C]): F[A, F[B, C]] = {
      splitP(F.lmap(f) { case (a, b) => a :: b })
    }

    def partialP[A, B, C, D, E](f: F[(A, B), D], ap: F[B, D] => E): F[A, E]

    def partialH[A, B <: HList, C, D, E](f: F[A :: B, D], ap: F[B, D] => E): F[A, E] = {
      partialP(F.lmap[A :: B, D, (A, B)](f) { case (a, b) => a :: b }, ap)
    }
  }

  implicit def stdRefreshInstanceForFunction1(implicit F0: Profunctor[? => ?]) = new ReFresh[? => ?] {
    val F = F0
    def combineP[A, B, C](f: A => B => C): ((A, B)) => C = {
      case (a, b) => f(a)(b)
    }

    def splitP[A, B, C](f: ((A, B)) => C): A => B => C = a => b => {
      f((a, b))
    }

    def partialP[A, B, C, D, E](f: ((A, B)) => D, ap: (B => D) => E): A => E = {
      a => ap(b => f((a, b)))
    }
  }

  type KlF[F[_]] = {
    type Aux[A, B] = Kleisli[F, A, B]
  }

  implicit def stdRefreshInstanceForKleisli[F[_]: Monad](implicit F0: Profunctor[KlF[F]#Aux]) = {
    new ReFresh[KlF[F]#Aux] {
      val F = F0
      def combineP[A, B, C](f: Kleisli[F, A, Kleisli[F, B, C]]): Kleisli[F, (A, B), C] = {
        Kleisli { case (a, b) => f.run(a).flatMap(_.run(b)) }
      }

      def splitP[A, B, C](f: Kleisli[F, (A, B), C]): Kleisli[F, A, Kleisli[F, B, C]] = {
        Kleisli(a => Kleisli((b: B) => f.run((a, b))).pure[F])
      }

      def partialP[A, B, C, D, E](f: Kleisli[F, (A, B), D], ap: Kleisli[F, B, D] => E): Kleisli[F, A, E] = {
        Kleisli(a => ap(f.local((a, _))).pure[F])
      }
    }
  }

  trait FuncF[T] extends DepFn1[T] with Serializable

  object FuncF {
    def apply[T](implicit func: FuncF[T]): Aux[T, func.Out] = func

    type Aux[T, Out0] = FuncF[T] { type Out = Out0 }

    implicit def fix[F[_, _]: Profunctor, A, B]: Aux[F[A :: HNil, B], F[A, B]] = {
      new FuncF[F[A :: HNil, B]] {
        type Out = F[A, B]

        def apply(f: F[A :: HNil, B]): F[A, B] = {
          f.lmap((_: A) :: HNil)
        }
      }
    }

    implicit def recurse[F[_, _], H, R <: HList, B](
      implicit
      fn: FuncF[F[R, B]],
      FS: ReFresh[F]
    ): Aux[F[H :: R, B], F[H, fn.Out]] = {
      new FuncF[F[H :: R, B]] {
        type Out = F[H, fn.Out]

        def apply(f: F[H :: R, B]): F[H, fn.Out] = {
          val ap: F[R, B] => fn.Out = fn(_)
          FS.partialH(f, ap)
        }
      }
    }
  }

  FuncF[Int :: HNil => String]
  FuncF[Int :: String :: Boolean :: HNil => String]

  trait ReFunc[T] extends DepFn1[T] with Serializable

  object ReFunc {
    def apply[T](implicit func: ReFunc[T]): Aux[T, func.Out] = func

    type Aux[T, Out0] = ReFunc[T] { type Out = Out0 }

    implicit def fix[A, B](
      implicit ev$: <:!<[B, (_) => _]
    ): Aux[A => B, A :: HNil => B] = {
      new ReFunc[A => B] {
        type Out = A :: HNil => B

        def apply(f: A => B): Out = {
          case a :: _ => f(a)
        }
      }
    }

    implicit def recurse[A, B, C, D, H <: HList](
      implicit fn: Aux[B => C, H => D]
    ): Aux[A => B => C, A :: H => D] = {
      new ReFunc[A => B => C] {
        type Out = A :: H => D

        def apply(f: A => B => C): A :: H => D = {
          case a :: h => fn(f(a))(h)
        }
      }
    }
  }

  val refuncAuxRec2: ReFunc.Aux[Int => Long => Boolean => String, Int :: Long :: Boolean :: HNil => String] = {
    ReFunc[Int => Long => Boolean => String]
  }

  val refuncAuxRec1: ReFunc.Aux[Int => Long => Boolean, Int :: Long :: HNil => Boolean] = {
    ReFunc[Int => Long => Boolean]
  }

  val refuncAuxFix: ReFunc.Aux[Int => Boolean, Int :: HNil => Boolean] = {
    ReFunc[Int => Boolean]
  }

  val funcAuxRec2: Func.Aux[Int :: Long :: Boolean :: HNil => String, Int => Long => Boolean => String] = {
    Func[Int :: Long :: Boolean :: HNil => String]
  }

  val funcAuxRec1: Func.Aux[Int :: Long :: HNil => String, Int => Long => String] = {
    Func[Int :: Long :: HNil => String]
  }

  val funcAuxFix: Func.Aux[Int :: HNil => String, Int => String] = {
    Func[Int :: HNil => String]
  }

  def funcTestA[L <: HList, B](f: L => B)(implicit func: Func[L => B]) = {
    func(f)
  }

  def main(args: Array[String]): Unit = {

    def safeSequence[A](futures: List[Future[A]])(implicit ec: ExecutionContext): Future[(List[Throwable], List[A])] = {
      futures.foldLeft(Future.successful(List.newBuilder[Throwable], List.newBuilder[A])) { (acc, elem) =>
        val addS = (succ: A) => acc.map { case (f, s) => (f, s += succ) }
        val addE = (ex: Throwable) => acc.map { case (f, s) => (f += ex, s) }
        elem.value.map(_.fold(addE, addS)).getOrElse(
          elem.flatMap(addS).recoverWith { case ex => addE(ex) }
        )
      }.map { case (f, s) => (f.result(), s.result()) }
    }


    def funcTest[A, B](f: A => B): B = ???
    def funcTest2[A, B, C](f: A => B => C): C = ???

    funcTest((_: Int) => (_: Long) => "")
    funcTest2((_: Int) => (_: Long) => (_: Boolean) => "")
    import shapeless.ops.hlist._
    implicitly[SelectAll[Int :: Long :: Boolean :: String :: HNil, Int :: Boolean ::HNil]].apply(3 :: 1L :: false :: "String" :: HNil)

    val a: String => String => Int = f1 => f2 => (f1.toInt + 1) * (f2.toInt + 1)
    val b: String => Int => String = f1 => f2 => ((f1.toInt + 1) * f2).toString
    val c1: Long => String => Boolean = _ => _ => true
    val d1: Boolean => Boolean => Char = _ => _ => 'r'

    val c: String => Kleisli[Id, Int, String] = _ => Kleisli[Id, Int, String](_ => "123")
    val d: String => Kleisli[Id, String, Boolean] = _ => Kleisli[Id, String, Boolean](_ => true)
    val e: String => Kleisli[Id, Boolean, Long] = _ => Kleisli[Id, Boolean, Long](_ => 0L)
    val f: String => Kleisli[Id, Long, String] = _ => Kleisli[Id, Long, String](_ => "1234")

    val testing0: ((String, String, Long, Boolean)) => String => Char = (b <**< a) >++> (d1 <**< c1)

    val testing1: ((String, String, Long, Boolean)) => String => Char = (d1 <**< c1) <++< (b <**< a)

    val testing2: ((String, String, String, String)) => Kleisli[Id, Int, String] = (d <**< c) >++> (f <**< e)

    val testing3: ((String, String, String, String)) => Kleisli[Id, Int, String] = (f <**< e) <++< (d <**< c)

    ()
  }


}
