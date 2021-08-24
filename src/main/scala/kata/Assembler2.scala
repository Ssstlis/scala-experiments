package kata

import util._
import scala.util.chaining._

object Assembler2 extends App {

  sealed trait Cmd extends Product with Serializable

  type RegOr = Either[String, Int]
  object Cmd {

    def of(string: String): Option[Cmd] =
      string.split("\\s").toList match {
        case "jmp" :: label :: _ => Some(Jmp(label))
        case "call" :: label :: _ => Some(Call(label))
        case "je" :: label :: _ => Some(Je(label))
        case "jne" :: label :: _ => Some(Jne(label))
        case "jge" :: label :: _ => Some(Jge(label))
        case "jg" :: label :: _ => Some(Jg(label))
        case "jl" :: label :: _ => Some(Jl(label))
        case "jle" :: label :: _ => Some(Jle(label))
        case "inc" :: reg :: _ => Some(Inc(reg))
        case "dec" :: reg :: _ => Some(Dec(reg))
        case "mov" :: reg :: value :: _ => Some(Mov(reg, value.toIntOption.toRight(value)))
        case "jnz" :: reg :: value :: _ => value.toIntOption.map(Jnz(reg.toIntOption.toRight(reg), _))
        case _ => None
      }

    final case class Inc(reg: String) extends Cmd
    final case class Dec(reg: String) extends Cmd
    final case class Mov(reg: String, regOr: RegOr) extends Cmd
    final case class Jnz(regOr: RegOr, value: Int) extends Cmd
    final case class Add(reg: String, regOr: RegOr) extends Cmd
    final case class Sub(reg: String, regOr: RegOr) extends Cmd
    final case class Mul(reg: String, regOr: RegOr) extends Cmd
    final case class Div(reg: String, regOr: RegOr) extends Cmd
    final case class Cmp(regOr1: RegOr, regOr2: RegOr) extends Cmd
    final case class Jmp(label: String) extends Cmd
    final case class Jne(label: String) extends Cmd
    final case class Je(label: String) extends Cmd
    final case class Jge(label: String) extends Cmd
    final case class Jg(label: String) extends Cmd
    final case class Jle(label: String) extends Cmd
    final case class Jl(label: String) extends Cmd
    final case class Call(label: String) extends Cmd
    final case object Ret extends Cmd
    final case object End extends Cmd
    final case class Msg(template: String) extends Cmd
  }

  def asCmds(list: List[String]) = list.flatMap(Cmd.of)

  def printSt(list: List[String]) = list.map(s => s"\"$s\"").toString()

  final case class State(output: Option[String] = None, regs: Map[String, Int] = Map.empty, comp: Option[Boolean])

  def splitted(input: String) = input.split("\\n").toList

  def splitlogic(input: List[String]) = input
    .map(_.pipe(s => s.indexOf(";").pipe(idx => if (idx != -1) s.take(idx) else s)).trim)
    .filter(_ != "")
    .collect {
      case s @ s"msg$_" => s
      case s => s.replaceAll("\\s+", " ")
    }
    .span(s => !s.endsWith(":"))
    .pipe(t => t.copy(_2 = t._2))

  def dropState[T](state: Option[(String, List[String])], acc: List[(String, ListZipper[T])])(
    f: List[String] => List[T]
  ) =
    state match {
      case Some((name, cmds)) => (name, ListZipper.of(f(cmds.reverse))) :: acc
      case _ => acc
    }

  def splitfn[T](input: List[String])(f: List[String] => List[T]) = tailRecM(
    (input.dropWhile(s => !s.endsWith(":")), Option.empty[(String, List[String])], List.empty[(String, ListZipper[T])])
  ) { case (rest, state, acc) =>
    rest match {
      case s"$fname:" :: next =>
        Left((next, Some((fname, List.empty)), dropState(state, acc)(f)))
      case cmd :: next =>
        Left((next, state.map(tp => tp.copy(_2 = cmd :: tp._2)), acc))
      case Nil => Right(dropState(state, acc)(f))
    }
  }

  def interpret(input: String): Option[String] = {
    println(input)
    val splitte = splitted(input)
    val (main, rest) = splitlogic(splitte).tap(println)

    asCmds(main).tap(println)

    splitfn(rest)(identity).tap(println)
    ???
  }

}
