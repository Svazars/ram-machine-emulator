package nsu.ram

import nsu.ram.Commands._

abstract class Executor(
                         val dataMemory: Memory,
                         val commandMemory: Memory,
                         val io: IOModule,
                         val instrSet: SupportedCommands,
                         val logger: Logger) extends Control {

  def nextCommand() : Unit

  def decodeCmd(binary: Register): Option[Command] = instrSet.decode(binary)

  def step(): ExecutionResult = {
    readContext() match {
      case Right(err) => err
      case Left((cmdBinary, arg)) =>
        decodeCmd(cmdBinary) match {
          case None => Error(s"Cannot decode $cmdBinary as a valid command")
          case Some(cmd) => execute(cmd, arg)
        }
    }
  }

  private def log(str: String): Unit = logger.log(str)
  private def getAccumulator: Register = dataMemory.readRegister(Register.ZERO)
  private def setAccumulator(value: Register): Unit = {
    log(s"  >> setAccumulator($value)")
    dataMemory.writeRegister(Register.ZERO, value)
  }

  private def readContext() : Either[(Register, Register), ExecutionResult] = {
    try {
      val pc = currentPC()
      log(s"currentPc = $pc")

      val cmdBinary = commandMemory.readRegister(pc)
      log(s"commandBinary = $cmdBinary")

      val arg = commandMemory.readRegister(pc.plusOne())
      log(s"argument = $arg")

      Left(cmdBinary, arg)
    } catch {
      case ExecutionException(msg) => Right(Error(msg))
    }
  }

  private def derefArg(arg: Register, cmd: Command) : Register = {
    val addrMode = cmd.tpe match {
      case Store | Read => cmd.addrMode match {
        case Constant => Constant
        case Direct => Constant
        case Indirect => Direct
      }

      case _ => cmd.addrMode
    }

    log(s"addrMode = $addrMode")
    addrMode match {
      case Constant =>
        log(s"immediate = $arg")
        arg

      case Direct =>
        log (s"dereferencing $arg")
        val data = dataMemory.readRegister(arg)
        log(s"*$arg = $data")
        data

      case Indirect =>
        log (s"dereferencing $arg")
        val ref = dataMemory.readRegister(arg)
        log (s"dereferencing $ref")
        val data = dataMemory.readRegister(ref)
        log(s"**$arg = *$ref = $data")
        data
    }
  }

  private def jumpTo(target: Register): Unit = updatePC(target.mul(Register.TWO).minus(Register.TWO))

  private def execute(cmd: Commands.Command, arg: Register) : ExecutionResult = {
    log(s"executing $cmd$arg")
    val effArgument = derefArg(arg, cmd)
    val acc = getAccumulator

    cmd.tpe match {
      case Load => setAccumulator(effArgument)
      case Store => dataMemory.writeRegister(effArgument, acc)

      case Add => setAccumulator(acc.plus(effArgument))
      case Sub => setAccumulator(acc.minus(effArgument))
      case Mult => setAccumulator(acc.mul(effArgument))

      case Div => acc.div(effArgument) match {
        case Some(r) => setAccumulator(r)
        case None => return Error("Division by zero")
      }

      case Read => io.in.read() match {
        case Some(data) => dataMemory.writeRegister(effArgument, data)
        case None => return Error("Read from empty input")
      }

      case Write => io.out.write(effArgument)

      case Jump => jumpTo(arg); return Success
      case JZero => if (acc.isZero) {
        jumpTo(arg)
        return Success
      }

      case JGTZ => if (acc.isPositive) {
        jumpTo(arg)
        return Success
      }

      case Halt => return HaltReached
    }

    nextCommand()
    Success
  }
}
