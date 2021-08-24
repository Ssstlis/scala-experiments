package kata

import kata.Assembler2.interpret
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Assembler2Spec extends AnyWordSpec with Matchers {

  "1" should {
    "1" in {

      val program = """
        |; My first program
        |mov  a, 5
        |inc  a
        |call function
        |msg  '(5+1)/2 = ', a    ; output message
        |end
        |
        |function:
        |    div  a, 2
        |    ret
        |""".stripMargin

      val expect = Some("(5+1)/2 = 3")

      interpret(program) mustBe expect
    }
  }

  "2" should {
    "1" in {

      val program = """
        |mov   a, 5
        |mov   b, a
        |mov   c, a
        |call  proc_fact
        |call  print
        |end
        |
        |proc_fact:
        |    dec   b
        |    mul   c, b
        |    cmp   b, 1
        |    jne   proc_fact
        |    ret
        |
        |print:
        |    msg   a, '! = ', c ; output text
        |    ret
        |""".stripMargin

      val expect = Some("5! = 120")

      interpret(program) mustBe expect
    }
  }

  "3" should {
    "1" in {

      val program =
        """
          |mov   a, 8            ; value
          |mov   b, 0            ; next
          |mov   c, 0            ; counter
          |mov   d, 0            ; first
          |mov   e, 1            ; second
          |call  proc_fib
          |call  print
          |end
          |
          |proc_fib:
          |    cmp   c, 2
          |    jl    func_0
          |    mov   b, d
          |    add   b, e
          |    mov   d, e
          |    mov   e, b
          |    inc   c
          |    cmp   c, a
          |    jle   proc_fib
          |    ret
          |
          |func_0:
          |    mov   b, c
          |    inc   c
          |    jmp   proc_fib
          |
          |print:
          |    msg   'Term ', a, ' of Fibonacci series is: ', b        ; output text
          |    ret
          |""".stripMargin

      val expect = Some("Term 8 of Fibonacci series is: 21")

      interpret(program) mustBe expect
    }
  }

  "4" should {
    "1" in {

      val program =
        """
          |mov   a, 11           ; value1
          |mov   b, 3            ; value2
          |call  mod_func
          |msg   'mod(', a, ', ', b, ') = ', d        ; output
          |end
          |
          |; Mod function
          |mod_func:
          |    mov   c, a        ; temp1
          |    div   c, b
          |    mul   c, b
          |    mov   d, a        ; temp2
          |    sub   d, c
          |    ret
          |""".stripMargin

      val expect = Some("mod(11, 3) = 2")

      interpret(program) mustBe expect
    }
  }

  "5" should {
    "1" in {

      val program =
        """
          |mov   a, 81         ; value1
          |mov   b, 153        ; value2
          |call  init
          |call  proc_gcd
          |call  print
          |end
          |
          |proc_gcd:
          |    cmp   c, d
          |    jne   loop
          |    ret
          |
          |loop:
          |    cmp   c, d
          |    jg    a_bigger
          |    jmp   b_bigger
          |
          |a_bigger:
          |    sub   c, d
          |    jmp   proc_gcd
          |
          |b_bigger:
          |    sub   d, c
          |    jmp   proc_gcd
          |
          |init:
          |    cmp   a, 0
          |    jl    a_abs
          |    cmp   b, 0
          |    jl    b_abs
          |    mov   c, a            ; temp1
          |    mov   d, b            ; temp2
          |    ret
          |
          |a_abs:
          |    mul   a, -1
          |    jmp   init
          |
          |b_abs:
          |    mul   b, -1
          |    jmp   init
          |
          |print:
          |    msg   'gcd(', a, ', ', b, ') = ', c
          |    ret
          |""".stripMargin

      val expect = Some("gcd(81, 153) = 9")

      interpret(program) mustBe expect
    }
  }

  "6" should {
    "1" in {

      val program =
        """
          |call  func1
          |call  print
          |end
          |
          |func1:
          |    call  func2
          |    ret
          |
          |func2:
          |    ret
          |
          |print:
          |    msg 'This program should return null'
          |""".stripMargin

      val expect = None

      interpret(program) mustBe expect
    }
  }

  "7" should {
    "1" in {

      val program =
        """
          |mov   a, 2            ; value1
          |mov   b, 10           ; value2
          |mov   c, a            ; temp1
          |mov   d, b            ; temp2
          |call  proc_func
          |call  print
          |end
          |
          |proc_func:
          |    cmp   d, 1
          |    je    continue
          |    mul   c, a
          |    dec   d
          |    call  proc_func
          |
          |continue:
          |    ret
          |
          |print:
          |    msg a, '^', b, ' = ', c
          |    ret
          |""".stripMargin

      val expect = Some("2^10 = 1024")

      interpret(program) mustBe expect
    }
  }
}