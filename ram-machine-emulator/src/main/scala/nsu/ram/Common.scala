package nsu.ram

import nsu.ram.Commands._

import scala.collection.mutable

case class Test(name: String, in: Array[Register], out: Array[Register])
case class Program(cmds: Seq[(Command, Register)])

trait Input {
  def read(): Option[Register]
}

trait Output {
  def write(data: Register): Unit
  def current(): Array[Register]
}

class BufferedOutput extends Output {
  private val buffer = mutable.ArrayBuffer[Register]()

  override def write(data: Register): Unit = buffer.append(data)
  override def current(): Array[Register] = buffer.toArray
  override def toString: String = buffer.map(_.value.bigInteger) mkString " | "
}

case class IOModule(in: Input, out: Output)


sealed abstract class ExecutionResult
case object Success extends ExecutionResult
case object OutOfIterations extends ExecutionResult
case object HaltReached extends ExecutionResult
case class Error(desc: String) extends ExecutionResult

trait Logger {
  def log(msg: String): Unit
}

object Logger {
  def toBuffer(buf: mutable.ArrayBuffer[String]) = new Logger {
    override def log(msg: String): Unit = buf.append(msg)
  }
}

trait AccessCheck {
  def checkIdx(idx: Register): Unit = {
    if (idx.isNegative) throw ExecutionException(s"Accessed register with idx ${idx.value}")
  }
}

trait Control extends AccessCheck {
  def currentPC(): Register
  def updatePC(newPC: Register) : Unit = {
    checkIdx(newPC)
    updatePC0(newPC)
  }

  protected def updatePC0(newPC: Register) : Unit
}

trait Memory extends AccessCheck {
  def readRegister(idx: Register): Register = {
    checkIdx(idx)
    readRegister0(idx)
  }

  def writeRegister(idx: Register, value: Register): Unit = {
    checkIdx(idx)
    writeRegister0(idx, value)
  }

  protected def readRegister0(idx: Register): Register
  protected def writeRegister0(idx: Register, value: Register): Unit
}

trait SupportedCommands {
  // def supportedCommands(): Seq[Command]
  def encode(cmd: Command): Register
  def decode(cmd: Register): Option[Command]
}

case class ExecutionException(reason: String) extends Exception(reason)

object InstructionSet {
  val DEFAULT = InstructionSet(Seq(
    Halt, Load, Store, Add, Sub, Mult, Div, Read, Write, Jump, JZero, JGTZ
  ))

  val DEFAULT_CODER = DEFAULT.asCommands()
}

case class InstructionSet(tpeSet: Seq[CommandType]) {

  def asCommands() : SupportedCommands = {
    val tpeOrdered: Seq[CommandType] = if (tpeSet contains Halt) {
      Seq(Halt) ++ tpeSet.filterNot(_ == Halt)
    } else {
      tpeSet
    }

    val cmds = mutable.ArrayBuffer[Command]()
    for (t <- tpeOrdered;
         a <- t.addrModes) {
      cmds += new Command(t, a)
    }

    val decodeTable = new Array[Command](cmds.size)
    val codeTable = mutable.Map[Command, Int]()

    assert(cmds.size < Int.MaxValue)
    for ((c, i) <- cmds.zipWithIndex) {
      decodeTable(i) = c
      codeTable(c) = i
    }
    assert(codeTable.size == decodeTable.length)

    new SupportedCommands {
      override def encode(cmd: Command): Register = {
        Register(codeTable(cmd))
      }
      override def decode(cmd: Register): Option[Command] = {
        if (cmd.isNegative || cmd.value >= BigInt(cmds.size)) {
          None
        } else {
          Some(decodeTable(cmd.value.toInt))
        }
      }

      //override def supportedCommands(): Seq[Command] = cmds
    }
  }
}

object Commands {
  class Command(val tpe: CommandType, val addrMode: AddressMode) {
    require(tpe.addrModes contains addrMode, s"Cannot use $addrMode for $tpe")

    private def isJump = tpe match {
      case Jump | JZero | JGTZ => true
      case _ => false
    }

    override def toString: String = tpe.name + (if (!isJump) addrMode match {
      case Constant => " ="
      case Direct => ""
      case Indirect => " *"
    } else "")

    override def equals(obj: scala.Any): Boolean = obj match {
      case other: Command => this.addrMode.equals(other.addrMode) && this.tpe.equals(other.tpe)
      case _ => false
    }

    override def hashCode(): Int = this.addrMode.hashCode() * 31 + this.tpe.hashCode()
  }

  sealed abstract class AddressMode
  case object Constant extends AddressMode
  case object Direct extends AddressMode
  case object Indirect extends AddressMode

  private val ALL_MODES = Seq(Constant, Direct, Indirect)
  private val CONSTANT_ONLY = Seq(Constant)

  sealed abstract class CommandType(val addrModes: Seq[AddressMode], val name: String)

  case object Load extends CommandType(ALL_MODES, "Load")
  case object Store extends CommandType(Seq(Direct, Indirect), "Store")

  case object Add extends CommandType(ALL_MODES, "Add")
  case object Sub extends CommandType(ALL_MODES, "Sub")
  case object Mult extends CommandType(ALL_MODES, "Mult")
  case object Div extends CommandType(ALL_MODES, "Div")

  case object Read extends CommandType(Seq(Direct, Indirect), "Read")
  case object Write extends CommandType(ALL_MODES, "Write")

  case object Jump extends CommandType(CONSTANT_ONLY, "Jump")
  case object JZero extends CommandType(CONSTANT_ONLY, "JZero")
  case object JGTZ extends CommandType(CONSTANT_ONLY, "JGTZ")

  case object Halt extends CommandType(Seq(Direct), "Halt")
}

case class Register(value: BigInt) {
  def isZero: Boolean = this.value.equals(Register.ZERO.value)
  def isPositive: Boolean = this.value > Register.ZERO.value
  def isNegative: Boolean = this.value < Register.ZERO.value

  def plusOne() : Register = this.plus(Register.ONE)

  def plus(other: Register) = Register(this.value + other.value)
  def minus(other: Register) = Register(this.value - other.value)
  def mul(other: Register) = Register(this.value * other.value)
  def div(other: Register) = if (other.isZero) None else Some(Register(this.value / other.value))
}
object Register {
  val ZERO = Register(BigInt(0))
  val ONE = Register(BigInt(1))
  val TWO = Register(BigInt(2))

  def apply(value: Int): Register = new Register(BigInt(value))
}

