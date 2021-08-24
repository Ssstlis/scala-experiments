package kata

import util._

object Assembler1 extends App {

  sealed trait Cmd extends Product with Serializable {
    def exec(regs: Map[String, Int], cmds: ListZipper[Cmd]) =
      this match {
        case Cmd.Inc(reg) => (regs.updatedWith(reg)(_.map(_ + 1)), cmds.right)
        case Cmd.Dec(reg) => (regs.updatedWith(reg)(_.map(_ - 1)), cmds.right)
        case Cmd.Mov(reg, value) =>
          val newState = value match {
            case Left(value) => regs.get(value).fold(regs)(regs.updated(reg, _))
            case Right(value) => regs.updated(reg, value)
          }
          (newState, cmds.right)
        case Cmd.Jnz(reg, value) =>
          val regValue = reg match {
            case Left(value) => regs.getOrElse(value, 0)
            case Right(value) => value
          }
          val nzip =
            if (regValue != 0)
              tailRecM((value, cmds)) {
                case (0, list) => Right(list)
                case (v, list) if v > 0 => Left((v - 1, list.right))
                case (v, list) => Left((v + 1, list.left))
              }
            else cmds.right
          (regs, nzip)
      }
  }

  object Cmd {
    def of(s: String): Option[Cmd] = {
      s.split("\\s").toList match {
        case "inc" :: reg :: _ => Some(Inc(reg))
        case "dec" :: reg :: _ => Some(Dec(reg))
        case "mov" :: reg :: value :: _ => Some(Mov(reg, value.toIntOption.toRight(value)))
        case "jnz" :: reg :: value :: _ => value.toIntOption.map(Jnz(reg.toIntOption.toRight(reg), _))
        case _ => None
      }
    }
    final case class Inc(reg: String) extends Cmd
    final case class Dec(reg: String) extends Cmd
    final case class Mov(reg: String, value: Either[String, Int]) extends Cmd
    final case class Jnz(reg: Either[String, Int], value: Int) extends Cmd
  }

  def asCmds(list: List[String]) = list.flatMap(Cmd.of)

  def interpret(cmds: List[String]): Map[String, Int] =
    tailRecM((Map.empty[String, Int], ListZipper.of(asCmds(cmds)).right)) { case (regs, cmds) =>
      if (cmds.end) Right(regs)
      else Left(cmds.head.fold((regs, cmds.right))(_.exec(regs, cmds)))
    }

}
