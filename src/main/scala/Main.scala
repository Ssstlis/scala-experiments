import scala.concurrent.{ExecutionContext, Future}

import Main.ReFunc.Aux
import cats.{Id, MonadError}
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import lib.Instances._
import lib.ReCompose.toReComposeOps
import lib.TupleOps._
import shapeless.{::, HNil, _}

object Main {

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
      implicit func: Func[R => B]
    ): Aux[H :: R => B, H => func.Out] = {
      new Func[H :: R => B] {
        type Out = H => func.Out

        def apply(t: H :: R => B): Out = h => {
          func(r => t(h :: r))
        }
      }
    }
  }

  trait ReFunc[T] extends DepFn1[T] with Serializable

  object ReFunc {
    def apply[T](implicit func: ReFunc[T]): Aux[T, func.Out] = func

    type Aux[T, Out0] = ReFunc[T] { type Out = Out0 }

    implicit def fix[A, B, C, D](
      implicit ev$: <:!<[B, C => D]
    ): Aux[A => B, A :: HNil => B] = {
      new ReFunc[A => B] {
        type Out = A :: HNil => B

        def apply(t: A => B): Out = {
          case a :: _ => t(a)
        }
      }
    }

    implicit def recurse[A, B, C, D, H <: HList](
      implicit
      func: Aux[B => C, H => D]
    ): Aux[A => B => C, A :: H => D] = {
      new ReFunc[A => B => C] {
        type Out = A :: H => D

        def apply(t: A => B => C): A :: H => D = {
          case a :: h => func(t(a))(h)
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
