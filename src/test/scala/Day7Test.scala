import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day7Test extends AnyFlatSpec with should.Matchers {
  val input =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin

  "ans" should "work" in {
    Day7.ans(input) should be(94853 + 584)
  }

  "ans2" should "work" in {
    Day7.ans2(input) should be(24933642)
  }
}
