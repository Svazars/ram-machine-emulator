package nsu.ram

object RAMMachine {
  def arrayInput(data: Array[Register]): Input = new Input {
    private val input = data.clone()
    private var current: Int = 0
    override def read(): Option[Register] = if (current < data.length) {
      current = current + 1
      Some(input(current - 1))
    } else {
      None
    }

    override def toString: String = {
      def pp(e: Register, id: Int) = if (id == current) {
        s" >${e.value}< "
      } else {
        s"${e.value}"
      }

      data.zipWithIndex.map { case (r, i) => pp(r, i) } mkString " | "
    }
  }

  private def emptyData(): Memory = new RAM()
  private def emptyOutput() : Output = new BufferedOutput()
  private def defaultIO(input: Input): IOModule = IOModule(input, emptyOutput())

  def programAsMemory(prog: Program): Memory = new Memory {
    override protected def readRegister0(idx: Register): Register = {
      val idInt: Int = idx.value.toInt // any reasonable program will fit in 2^32 cmds
      if (idInt >= 2 * prog.cmds.size) {
        throw ExecutionException(
          s"$idInt is not a valid label: current program consists of ${prog.cmds.size} commands")
      }

      if (idInt % 2 == 0) {
        InstructionSet.DEFAULT_CODER.encode(prog.cmds(idInt / 2)._1)
      } else {
        prog.cmds(idInt / 2)._2
      }
    }

    override protected def writeRegister0(idx: Register, value: Register): Unit =
      throw new AssertionError(s"Tried to rewrite program")
  }
}

final class RAMMachine(instrSet: InstructionSet, prog: Program, input: Input, logger: Logger)
  extends Executor(
    RAMMachine.emptyData(),
    RAMMachine.programAsMemory(prog),
    RAMMachine.defaultIO(input),
    instrSet.asCommands(),
    logger
  ) with RAMControl {

  def printState(): String = {
    val printer = new StringBuilder

    val c = commandMemory.readRegister(currentPC())
    val a = commandMemory.readRegister(currentPC().plusOne())

    printer.append("Input     :").append(io.in).append("\n")
           .append("Output    :").append(io.out).append("\n\n")
           .append("Registers :").append(dataMemory).append("\n\n")
           .append(s"PC             :  ${currentPC().div(Register.TWO).get.plus(Register.ONE).value}\n")
           .append(s"Current command:  ${c.value} ${a.value}\n")
           .append(s"               :  ${decodeCmd(c).get} ${a.value}\n")
      .toString
  }

  override def nextCommand(): Unit = updatePC(currentPC().plus(Register.TWO))
}

trait RAMControl extends Control {
  private var current: Register = Register.ZERO

  override def currentPC(): Register = current
  override def updatePC0(newPC: Register): Unit = {
    current = newPC
  }
}

final class RAM extends Memory {
  private val memory = scala.collection.mutable.Map[Register, Register]()

  protected override def readRegister0(idx: Register): Register = {
    memory.getOrElse(idx, Register.ZERO)
  }

  protected override def writeRegister0(idx: Register, value: Register): Unit = {
    memory(idx) = value
  }

  override def toString: String = if (memory.nonEmpty) {
    val max = memory.keys.map(_.value).max
    (0 to max.toInt).map { i =>
      val e = memory.getOrElse(Register(i), Register.ZERO)
      s"R_$i = ${e.value}"
    } mkString " | "
  } else {
    ""
  }
}
