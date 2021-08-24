package kata

import kata.Assembler1.interpret
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Assembler1Spec extends AnyWordSpec with Matchers {

  "interpret(List(mov a -10, mov b a, inc a, dec b, jnz a -2))" should {
    "return Map(a -> 0, b -> -20)" in {
      interpret(List("mov a -10", "mov b a", "inc a", "dec b", "jnz a -2")) mustBe Map("a" -> 0, "b" -> (-20))
    }
  }

  "interpret(List(mov a 5, inc a, dec a, dec a, jnz a -1, inc a))" should {
    "return Map(a -> 1)" in {
      interpret(List("mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a")) mustBe Map("a" -> 1)
    }
  }

  "interpret(List(mov a 1, mov b -2, mov c 45))" should {
    "return Map(a -> 1, b -> -2, c -> 45)" in {
      interpret(List("mov a 1", "mov b -2", "mov c 45")) mustBe Map("a" -> 1, "b" -> -2, "c" -> 45)
    }
  }

  "interpret(List(mov a 1, mov b 1, mov c 0, mov d 26, jnz c 2, jnz 1 5, mov c 7, inc d, dec c, jnz c -2, mov c a, inc a, dec b, jnz b -2, mov b c, dec d, jnz d -6, mov c 18, mov d 11, inc a, dec d, jnz d -2, dec c, jnz c -5))" should {
    "return Map(a -> 318009, b -> 196418, c -> 0, d -> 0)" in {
      interpret(
        List(
          "mov a 1",
          "mov b 1",
          "mov c 0",
          "mov d 26",
          "jnz c 2",
          "jnz 1 5",
          "mov c 7",
          "inc d",
          "dec c",
          "jnz c -2",
          "mov c a",
          "inc a",
          "dec b",
          "jnz b -2",
          "mov b c",
          "dec d",
          "jnz d -6",
          "mov c 18",
          "mov d 11",
          "inc a",
          "dec d",
          "jnz d -2",
          "dec c",
          "jnz c -5"
        )
      ) mustBe Map("a" -> 318009, "b" -> 196418, "c" -> 0, "d" -> 0)
    }
  }

  "interpret(List(mov s 33, mov o 27, mov g 28, mov c 38, mov h 34, mov z 0, jnz z 4, jnz s 3, inc s 0, dec s 1, inc s 2, dec s 3, inc o 0, inc o 1, dec o 2, inc o 3, inc g 0, dec g 1, dec g 2, inc c 0, inc c 1, inc c 2, inc h 0, inc h 1))" should {
    "return Map(s -> 33, g -> 27, c -> 41, h -> 36, o -> 29, z -> 0)" in {
      interpret(
        List(
          "mov s 33",
          "mov o 27",
          "mov g 28",
          "mov c 38",
          "mov h 34",
          "mov z 0",
          "jnz z 4",
          "jnz s 3",
          "inc s 0",
          "dec s 1",
          "inc s 2",
          "dec s 3",
          "inc o 0",
          "inc o 1",
          "dec o 2",
          "inc o 3",
          "inc g 0",
          "dec g 1",
          "dec g 2",
          "inc c 0",
          "inc c 1",
          "inc c 2",
          "inc h 0",
          "inc h 1"
        )
      ) mustBe Map("s" -> 33, "g" -> 27, "c" -> 41, "h" -> 36, "o" -> 29, "z" -> 0)
    }
  }

  "interpret(List(List(mov c 21, mov o 29, mov z 1, jnz z 2, jnz c 4, inc c 0, dec c 1, inc c 2, inc c 3, inc o 0, inc o 1, inc o 2))" should {
    "return Map(c -> 23, o -> 32, z -> 1)" in {
      interpret(
        List(
          "mov c 21",
          "mov o 29",
          "mov z 1",
          "jnz z 2",
          "jnz c 4",
          "inc c 0",
          "dec c 1",
          "inc c 2",
          "inc c 3",
          "inc o 0",
          "inc o 1",
          "inc o 2"
        )
      ) mustBe Map("c" -> 23, "o" -> 32, "z" -> 1)
    }
  }

  "interpret(List(mov o 20, mov g 27, mov c 21, mov n 24, mov z 0, jnz z 4, jnz o 4, dec o 0, inc o 1, inc o 2, inc o 3, inc g 0, dec g 1, inc g 2, inc c 0, inc c 1, dec c 2, dec n 0, inc n 1, dec n 2, inc n 3))" should {
    "return Map(n -> 24, g -> 28, c -> 22, o -> 21, z -> 0)" in {
      interpret(
        List(
          "mov o 20",
          "mov g 27",
          "mov c 21",
          "mov n 24",
          "mov z 0",
          "jnz z 4",
          "jnz o 4",
          "dec o 0",
          "inc o 1",
          "inc o 2",
          "inc o 3",
          "inc g 0",
          "dec g 1",
          "inc g 2",
          "inc c 0",
          "inc c 1",
          "dec c 2",
          "dec n 0",
          "inc n 1",
          "dec n 2",
          "inc n 3"
        )
      ) mustBe Map("n" -> 24, "g" -> 28, "c" -> 22, "o" -> 21, "z" -> 0)
    }
  }

  "interpret(List(mov g 29, mov o 30, mov c 32, mov n 22, mov z 0, jnz z 2, jnz g 2, inc g 0, dec g 1, inc o 0, inc o 1, dec o 2, inc c 0, inc c 1, inc c 2, dec n 0, inc n 1))" should {
    "return Map(n -> 22, g -> 28, c -> 35, o -> 31, z -> 0)" in {
      interpret(
        List(
          "mov g 29",
          "mov o 30",
          "mov c 32",
          "mov n 22",
          "mov z 0",
          "jnz z 2",
          "jnz g 2",
          "inc g 0",
          "dec g 1",
          "inc o 0",
          "inc o 1",
          "dec o 2",
          "inc c 0",
          "inc c 1",
          "inc c 2",
          "dec n 0",
          "inc n 1"
        )
      ) mustBe Map("n" -> 22, "g" -> 28, "c" -> 35, "o" -> 31, "z" -> 0)
    }
  }
}
