package nsu.ram.parsing

import nsu.ram.Commands.Command
import nsu.ram.{InstructionSet, Program, Register, SupportedCommands}

object BinaryParser {
  val DEFAULT = new BinaryParser(InstructionSet.DEFAULT.asCommands())
}

class BinaryParser(val instructionSet: SupportedCommands) extends Parser(";") {
  private def asInts(str: String): Iterable[BigInt] = {
    str.split(" ").flatMap(Parser.trim).map(BigInt(_))
  }

  private def asCommand(s: BigInt): Command = instructionSet.decode(Register(s)) match {
    case Some(cmd) => cmd
    case None => throw new AssertionError(s"Cannot decode $s as a valid command")
  }

  def parse(path: String): Program = {
    val ints = readFile(path).flatMap(asInts)
    Program(ints.sliding(2, 2).map { case Seq(cmd, arg) => (asCommand(cmd), Register(arg))}.toSeq)
  }
}
