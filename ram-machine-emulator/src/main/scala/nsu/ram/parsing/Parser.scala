package nsu.ram.parsing

import scala.io.Source

object Parser {
  def trim(str: String): Option[String] = Some(str.trim).flatMap(s => if (s.isEmpty) None else Some(s))
}

class Parser(val endOfLineCommentSymbol: String) {
  protected def readFile(path: String): Iterator[String] = Source.fromFile(path).getLines.flatMap(processLine)

  private def processLine(str: String): Option[String] = Parser.trim(
    str.indexOf(endOfLineCommentSymbol) match {
      case -1 => str
      case n => str.substring(0, n)
    })
}
