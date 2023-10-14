package nsu.ram.parsing

import nsu.ram.Commands._
import nsu.ram.parsing.AsmParser.Label
import nsu.ram.{InstructionSet, Program, Register}

import scala.collection.mutable

object AsmParser {
  val DEFAULT = new AsmParser(InstructionSet.DEFAULT)

  case class Label(v: String)
}

trait ParsedAnyCommand
trait ParsedBasicCommand extends ParsedAnyCommand {
  def cmd: Command
}

case class ParsedCommandReg(override val cmd: Command, r: Register) extends ParsedBasicCommand
case class ParsedCommandLabel(override val cmd: Command, l: Label) extends ParsedBasicCommand
case class ParsedLabeledCommand(p: ParsedBasicCommand, l: Label) extends ParsedAnyCommand

class AsmParser(val instrSet: InstructionSet) extends Parser(";") {
  private val supportedCmds = instrSet.asCommands()
  private val name2cmd: Map[String, CommandType] = instrSet.tpeSet.map(x => (x.name.toLowerCase, x)).toMap

  private def parseCommandFromString(s: String): ParsedAnyCommand = s.split(":") match {
    case Array(s) => parseCmd(s)
    case Array(label, s) => ParsedLabeledCommand(parseCmd(s), Label(label))
    case _ => throw new AssertionError(s"$s is not <label:cmd> nor <cmd>")
  }

  private def parseCmd(s: String): ParsedBasicCommand = {
    def tryParseRegisterNum(str: String): Option[(BigInt, AddressMode)] = {
      try {
        val (raw, amode) = if (str.startsWith("=")) {
          (str.substring(1), Constant)
        } else if (str.startsWith("*")) {
          (str.substring(1), Indirect)
        } else {
          (str, Direct)
        }

        Some((BigInt(raw), amode))
      } catch {
        case _: Exception => None
      }
    }

    s.split(" ").flatMap(Parser.trim) match {
      case Array(cmd, modInt) =>
        name2cmd.get(cmd.toLowerCase) match {
          case Some(tpe) => tpe match {
            case Jump | JZero | JGTZ =>
              tryParseRegisterNum(modInt) match {
                case Some((r, Direct)) => ParsedCommandReg(new Command(tpe, Constant), Register(r))
                case None => ParsedCommandLabel(new Command(tpe, Constant), Label(modInt))
              }

            case _ =>
              tryParseRegisterNum(modInt) match {
                case Some((r, mode)) => ParsedCommandReg(new Command(tpe, mode), Register(r))
                case _ => throw new AssertionError(s"$s -- cannot use label for cmd of type $tpe")
              }
          }
          case _ => throw new AssertionError(s"Command $cmd is not supported")
        }

      case _ => throw new AssertionError(s"$s is not <cmd {modifier}int>")
    }
  }

  def parse(program: java.util.Iterator[String]): (Program, Map[Label, Register]) = {
    import scala.jdk.CollectionConverters._
    parse(program.asScala)
  }

  def parse(program: Iterator[String]): (Program, Map[Label, Register]) = {
    val labelMap = mutable.Map[Label, Register]()

    val progWithResolvedLabels = program
      .map(parseCommandFromString)
      .zipWithIndex
      .map {
        case (ParsedLabeledCommand(c, l), i) =>
          labelMap(l) = Register(i + 1) // remember label position and "unfold" ParsedLabeledCommand into plain command
          c

        case (c, _) => c
      }
      .toSeq // need to drain iterator to evaluate labelMap
      .map {
        case ParsedCommandLabel(cmd, lbl) =>
          assert(labelMap contains lbl, s"$lbl is used in instruction but undefined")
          (cmd, labelMap(lbl))

        case ParsedCommandReg(cmd, reg) => (cmd, reg)
      }

    (Program(progWithResolvedLabels), labelMap.toMap)
  }

  def parse(path: String): (Program, Map[Label, Register]) = parse(readFile(path))

  def write(prog: Program, labelMap: Map[Label, Register]): String = {

    def padS(v: String): String = v.padTo(4, " ") mkString ""

    def pad(v: BigInt): String = padS(v.toString)

    def labeled(idx: Int): Option[Label] = {
      labelMap.iterator.find {
        case (_, lblIdx) => lblIdx.equals(Register(idx))
      } match {
        case Some((l, _)) => Some(l)
        case None => None
      }
    }

    def prettyPrint(cmd: Command, arg: Register) = cmd.tpe match {
      case Jump | JZero | JGTZ => s"$cmd" + (labeled(arg.value.toInt) match {
        case None => " " + arg.value
        case Some(x) => " " + x.v
      })
      case _ => s"$cmd ${arg.value}"
    }

    val sBuilder = new mutable.StringBuilder()

    for (((cmd, arg), i) <- prog.cmds.zipWithIndex) {
      labeled(i + 1) match {
        case Some(l) =>
          sBuilder.append("\n")
            .append(s"            ; ${l.v}:\n")
        case None =>
      }
      sBuilder.append(s"${pad(supportedCmds.encode(cmd).value)} ${pad(arg.value)}   ;   ${padS((i + 1) + ".")}   ${prettyPrint(cmd, arg)}\n")
    }

    sBuilder.toString
  }
}
