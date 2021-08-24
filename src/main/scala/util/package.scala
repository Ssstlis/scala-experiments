import scala.annotation.tailrec

package object util {
  @tailrec def tailRecM[A, B](a: A)(f: A => Either[A, B]): B =
    f(a) match {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => b
    }

  implicit class ListOps[A](private val list: List[A]) {
    def uncons: (Option[A], List[A]) =
      list match {
        case head :: tl => (Some(head), tl)
        case _ => (None, Nil)
      }
  }

  final case class ListZipper[A](private val prev: List[A], head: Option[A], private val next: List[A]) {
    def right: ListZipper[A] = {
      val nprev = head.fold(prev)(_ :: prev)
      val (nhead, nnext) = next.uncons
      ListZipper(nprev, nhead, nnext)
    }
    def left: ListZipper[A] = {
      val nnext = head.fold(List.empty[A])(_ :: next)
      val (nhead, nprev) = prev.uncons
      ListZipper(nprev, nhead, nnext)
    }
    def end: Boolean = head.isEmpty && next.isEmpty
  }

  object ListZipper {
    def of[A](list: List[A]) = ListZipper(Nil, None, list)
  }
  type StringListZipper = ListZipper[String]
}
