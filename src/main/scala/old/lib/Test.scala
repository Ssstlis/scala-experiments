package old.lib

object Test extends App {

  def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean =
    seq2.diff(seq1.map(v => v * v)).isEmpty

  def count(string: String): Map[Char, Int] =
    string.groupBy(identity).view.mapValues(_.length).toMap
  // Implement me! :)

  val seq1 = Seq(121, 144, 19, 161, 19, 144, 19, 11)
  val seq2 = Seq(11 * 11, 121 * 121, 144 * 144, 19 * 19, 161 * 161, 19 * 19, 144 * 144, 19 * 19)
  println(0 ^ 1)
  assert(comp(seq1, seq2), s"\ncomp($seq1, $seq2) should be true")
}
