package nsu.ram.parsing

import nsu.ram.{Register, Test}

object TestParser {
  val DEFAULT = new TestParser
}

class TestParser extends Parser("//") {

  private def case2arr(caseStr: String): Array[Register] = caseStr.split(" ").flatMap(Parser.trim).map(r => Register(BigInt(r)))

  private def str2case(str: String): Test = str.split(":").flatMap(Parser.trim) match {
    case Array(name, rest) =>
      rest.split("->").flatMap(Parser.trim) match {
        case Array(in, out) => Test(name, case2arr(in), case2arr(out))
        case _ => throw new AssertionError(s"$str is not a valid test case, expected <name>: in -> out")
      }
    case _ => throw new AssertionError(s"$str is not a valid test case, expected <name>: in -> out")
  }

  def parse(path: String): Iterator[Test] = readFile(path).map(str2case)
}

