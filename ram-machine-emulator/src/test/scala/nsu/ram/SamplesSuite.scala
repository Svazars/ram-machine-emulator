package nsu.ram

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.io.Source

class SamplesSuite extends AnyFunSuiteLike {

  private val CWD = System.getProperty("user.dir")

  private case class Logger() extends ((String) => Unit) {
    private val buf = new StringBuilder()
    override def apply(v1: String): Unit = buf.append(v1)
    def getLog: String = buf.toString()
  }

  private def doRun(params: Iterable[String], expected: String) = {
    val l = Logger()
    Main.setLogger(l)
    Main.main(params.toArray)
    assert(l.getLog == expected)
  }

  private def expectFile(path: String, expected: String): Unit = {
    val real = Source.fromFile(path).getLines() mkString "\n"
    assert(real == expected)
  }

  test("Empty params") {
    doRun(List.empty,
      s"""run examples:
         |  <file>.asm              -- convert sources to binary format
         |  <file>.bin "1 2 3" 1000 -- start program with input <read_0=1 read_1=2 read_2=3>, at most 1000 iterations
         |  --test <file1>.bin <file2>.test 1000 -- test program with all inputs, at most 1000 iterations per test
         |""".stripMargin)
  }

  test("InstructionSet test") {

    doRun(List("samples/InstructionSet/Set.bin", "", "1"),
    s"""Reading program from $CWD/samples/InstructionSet/Set.bin
        |Program:
        |============
        |0    0      ;   1.     Halt 0
        |1    1      ;   2.     Load = 1
        |2    2      ;   3.     Load 2
        |3    3      ;   4.     Load * 3
        |4    4      ;   5.     Store 4
        |5    5      ;   6.     Store * 5
        |6    6      ;   7.     Add = 6
        |7    7      ;   8.     Add 7
        |8    8      ;   9.     Add * 8
        |9    9      ;   10.    Sub = 9
        |10   10     ;   11.    Sub 10
        |11   11     ;   12.    Sub * 11
        |12   12     ;   13.    Mult = 12
        |13   13     ;   14.    Mult 13
        |14   14     ;   15.    Mult * 14
        |15   15     ;   16.    Div = 15
        |16   16     ;   17.    Div 16
        |17   17     ;   18.    Div * 17
        |18   18     ;   19.    Read 18
        |19   19     ;   20.    Read * 19
        |20   20     ;   21.    Write = 20
        |21   21     ;   22.    Write 21
        |22   22     ;   23.    Write * 22
        |23   23     ;   24.    Jump 23
        |24   24     ;   25.    JZero 24
        |25   25     ;   26.    JGTZ 25
        |
        |============
        |Running with
        |  input : empty
        |  maxIterations = 1
        |====================================================
        |Iteration 0:
        |Input     :
        |Output    :
        |
        |Registers :
        |
        |PC             :  1
        |Current command:  0 0
        |               :  Halt 0
        |
        |HALT executed, stopping
        |Final output:
        |""".stripMargin
    )
  }

  test("Euclid compile") {
    val PROG_SOURCE =
      """Read 1
        |Read 2
        |
        |LOOP: Load 1
        |      Sub  2 ; r_0 = r_1 - r_2
        |      JZero FINISH
        |      JGTZ  G
        |      Jump  L
        |
        |G:    Store 1
        |      Jump LOOP
        |
        |L:    MULT =-1 ; r_0 = r_2 - r_1
        |      Store 2
        |      Jump LOOP
        |
        |FINISH: Write 1
        |        Halt 0""".stripMargin

    val EXPECTED_ASM =
      """18   1      ;   1.     Read 1
        |18   2      ;   2.     Read 2
        |
        |            ; LOOP:
        |2    1      ;   3.     Load 1
        |10   2      ;   4.     Sub 2
        |24   13     ;   5.     JZero FINISH
        |25   8      ;   6.     JGTZ G
        |23   10     ;   7.     Jump L
        |
        |            ; G:
        |4    1      ;   8.     Store 1
        |23   3      ;   9.     Jump LOOP
        |
        |            ; L:
        |12   -1     ;   10.    Mult = -1
        |4    2      ;   11.    Store 2
        |23   3      ;   12.    Jump LOOP
        |
        |            ; FINISH:
        |21   1      ;   13.    Write 1
        |0    0      ;   14.    Halt 0""".stripMargin

    val programPath = CWD + "/samples/Euclid/Euclid.asm"

    expectFile(programPath, PROG_SOURCE)
    doRun(List(programPath),
        s"""Parsing $CWD/samples/Euclid/Euclid.asm
        |Overwriting $CWD/samples/Euclid/Euclid.bin
        |""".stripMargin
    )
    expectFile("samples/Euclid/Euclid.bin", EXPECTED_ASM)
  }

  test("Euclid test") {
    doRun(List("--test", "samples/Euclid/Euclid.bin", "samples/Euclid/Euclid.tests", "10000"),
    s"""Testing
        |Reading program from $CWD/samples/Euclid/Euclid.bin
        |Program:
        |============
        |18   1      ;   1.     Read 1
        |18   2      ;   2.     Read 2
        |2    1      ;   3.     Load 1
        |10   2      ;   4.     Sub 2
        |24   13     ;   5.     JZero 13
        |25   8      ;   6.     JGTZ 8
        |23   10     ;   7.     Jump 10
        |4    1      ;   8.     Store 1
        |23   3      ;   9.     Jump 3
        |12   -1     ;   10.    Mult = -1
        |4    2      ;   11.    Store 2
        |23   3      ;   12.    Jump 3
        |21   1      ;   13.    Write 1
        |0    0      ;   14.    Halt 0
        |============
        |Max iterations = 10000
        |Reading tests from $CWD/samples/Euclid/Euclid.tests
        |  tests = 7
        |1 / 7 test 2 2 ...
        | 7 iterations.   Success
        |2 / 7 test 2 4 ...
        | 15 iterations.   Success
        |3 / 7 test 3 7 ...
        | 35 iterations.   Success
        |4 / 7 test 2 2 ...
        | 7 iterations.   Success
        |5 / 7 test ...
        | 3055 iterations.   Success
        |6 / 7 test ...
        | 3181 iterations.   Success
        |7 / 7 test ...
        | 355 iterations.   Success
        |""".stripMargin
    )
  }

  test("Euclid2 test") {
    doRun(List("--test", "samples/Euclid2/Euclid.bin", "samples/Euclid2/Euclid.tests", "10000"),
      s"""Testing
        |Reading program from $CWD/samples/Euclid2/Euclid.bin
        |Program:
        |============
        |18   1      ;   1.     Read 1
        |18   2      ;   2.     Read 2
        |2    1      ;   3.     Load 1
        |10   2      ;   4.     Sub 2
        |24   12     ;   5.     JZero 12
        |25   10     ;   6.     JGTZ 10
        |12   -1     ;   7.     Mult = -1
        |4    2      ;   8.     Store 2
        |23   3      ;   9.     Jump 3
        |4    1      ;   10.    Store 1
        |23   3      ;   11.    Jump 3
        |21   1      ;   12.    Write 1
        |0    0      ;   13.    Halt 0
        |============
        |Max iterations = 10000
        |Reading tests from $CWD/samples/Euclid2/Euclid.tests
        |  tests = 7
        |1 / 7 test 2 2 ...
        | 7 iterations.   Success
        |2 / 7 test 2 4 ...
        | 14 iterations.   Success
        |3 / 7 test 3 7 ...
        | 33 iterations.   Success
        |4 / 7 test 2 2 ...
        | 7 iterations.   Success
        |5 / 7 test ...
        | 3055 iterations.   Success
        |6 / 7 test ...
        | 3178 iterations.   Success
        |7 / 7 test ...
        | 355 iterations.   Success
        |""".stripMargin
    )
  }

  test("Printer run") {
    doRun(List("samples/Printer/Printer.bin", "", "1000"),
      s"""Reading program from $CWD/samples/Printer/Printer.bin
         |Program:
         |============
         |20   0      ;   1.     Write = 0
         |20   1      ;   2.     Write = 1
         |20   2      ;   3.     Write = 2
         |20   3      ;   4.     Write = 3
         |1    1      ;   5.     Load = 1
         |4    1      ;   6.     Store 1
         |6    1      ;   7.     Add = 1
         |4    1      ;   8.     Store 1
         |5    1      ;   9.     Store * 1
         |25   16     ;   10.    JGTZ 16
         |21   0      ;   11.    Write 0
         |21   1      ;   12.    Write 1
         |21   2      ;   13.    Write 2
         |21   3      ;   14.    Write 3
         |0    0      ;   15.    Halt 0
         |10   1      ;   16.    Sub 1
         |23   11     ;   17.    Jump 11
         |
         |============
         |Running with
         |  input : empty
         |  maxIterations = 1000
         |====================================================
         |Iteration 0:
         |Input     :
         |Output    :
         |
         |Registers :
         |
         |PC             :  1
         |Current command:  20 0
         |               :  Write = 0
         |
         |====================================================
         |Iteration 1:
         |Input     :
         |Output    :0
         |
         |Registers :
         |
         |PC             :  2
         |Current command:  20 1
         |               :  Write = 1
         |
         |====================================================
         |Iteration 2:
         |Input     :
         |Output    :0 | 1
         |
         |Registers :
         |
         |PC             :  3
         |Current command:  20 2
         |               :  Write = 2
         |
         |====================================================
         |Iteration 3:
         |Input     :
         |Output    :0 | 1 | 2
         |
         |Registers :
         |
         |PC             :  4
         |Current command:  20 3
         |               :  Write = 3
         |
         |====================================================
         |Iteration 4:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :
         |
         |PC             :  5
         |Current command:  1 1
         |               :  Load = 1
         |
         |====================================================
         |Iteration 5:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 1
         |
         |PC             :  6
         |Current command:  4 1
         |               :  Store 1
         |
         |====================================================
         |Iteration 6:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 1 | R_1 = 1
         |
         |PC             :  7
         |Current command:  6 1
         |               :  Add = 1
         |
         |====================================================
         |Iteration 7:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 2 | R_1 = 1
         |
         |PC             :  8
         |Current command:  4 1
         |               :  Store 1
         |
         |====================================================
         |Iteration 8:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 2 | R_1 = 2
         |
         |PC             :  9
         |Current command:  5 1
         |               :  Store * 1
         |
         |====================================================
         |Iteration 9:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 2 | R_1 = 2 | R_2 = 2
         |
         |PC             :  10
         |Current command:  25 16
         |               :  JGTZ 16
         |
         |====================================================
         |Iteration 10:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 2 | R_1 = 2 | R_2 = 2
         |
         |PC             :  16
         |Current command:  10 1
         |               :  Sub 1
         |
         |====================================================
         |Iteration 11:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  17
         |Current command:  23 11
         |               :  Jump 11
         |
         |====================================================
         |Iteration 12:
         |Input     :
         |Output    :0 | 1 | 2 | 3
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  11
         |Current command:  21 0
         |               :  Write 0
         |
         |====================================================
         |Iteration 13:
         |Input     :
         |Output    :0 | 1 | 2 | 3 | 0
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  12
         |Current command:  21 1
         |               :  Write 1
         |
         |====================================================
         |Iteration 14:
         |Input     :
         |Output    :0 | 1 | 2 | 3 | 0 | 2
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  13
         |Current command:  21 2
         |               :  Write 2
         |
         |====================================================
         |Iteration 15:
         |Input     :
         |Output    :0 | 1 | 2 | 3 | 0 | 2 | 2
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  14
         |Current command:  21 3
         |               :  Write 3
         |
         |====================================================
         |Iteration 16:
         |Input     :
         |Output    :0 | 1 | 2 | 3 | 0 | 2 | 2 | 0
         |
         |Registers :R_0 = 0 | R_1 = 2 | R_2 = 2
         |
         |PC             :  15
         |Current command:  0 0
         |               :  Halt 0
         |
         |HALT executed, stopping
         |Final output:0 | 1 | 2 | 3 | 0 | 2 | 2 | 0
         |""".stripMargin
    )
  }

  test("Random test") {
    doRun(List("samples/RandomStringAsAProgram/Random.bin", "", "1"),
      s"""Reading program from $CWD/samples/RandomStringAsAProgram/Random.bin
         |Program:
         |============
         |8    57     ;   1.     Add * 57
         |3    428    ;   2.     Load * 428
         |7    5      ;   3.     Add 5
         |4    95     ;   4.     Store 95
         |4    81     ;   5.     Store 81
         |21   0      ;   6.     Write 0
         |25   6      ;   7.     JGTZ 6
         |14   5      ;   8.     Mult * 5
         |6    12     ;   9.     Add = 12
         |3    758    ;   10.    Load * 758
         |1    3      ;   11.    Load = 3
         |4    5      ;   12.    Store 5
         |12   387    ;   13.    Mult = 387
         |1    2      ;   14.    Load = 2
         |3    4      ;   15.    Load * 4
         |7    5      ;   16.    Add 5
         |1    8      ;   17.    Load = 8
         |7    1      ;   18.    Add 1
         |4    5      ;   19.    Store 5
         |7    4      ;   20.    Add 4
         |5    0      ;   21.    Store * 0
         |9    12     ;   22.    Sub = 12
         |4    8      ;   23.    Store 8
         |0    9      ;   24.    Halt 9
         |6    3      ;   25.    Add = 3
         |4    8      ;   26.    Store 8
         |5    7      ;   27.    Store * 7
         |1    2      ;   28.    Load = 2
         |0    8      ;   29.    Halt 8
         |0    9      ;   30.    Halt 9
         |5    1      ;   31.    Store * 1
         |6    7      ;   32.    Add = 7
         |5    12     ;   33.    Store * 12
         |7    0      ;   34.    Add 0
         |4    5      ;   35.    Store 5
         |7    1      ;   36.    Add 1
         |3    4      ;   37.    Load * 4
         |8    95     ;   38.    Add * 95
         |1    9      ;   39.    Load = 9
         |8    47     ;   40.    Add * 47
         |5    5      ;   41.    Store * 5
         |
         |============
         |Running with
         |  input : empty
         |  maxIterations = 1
         |====================================================
         |Iteration 0:
         |Input     :
         |Output    :
         |
         |Registers :
         |
         |PC             :  1
         |Current command:  8 57
         |               :  Add * 57
         |
         |====================================================
         |Iteration 1:
         |Input     :
         |Output    :
         |
         |Registers :R_0 = 0
         |
         |PC             :  2
         |Current command:  3 428
         |               :  Load * 428
         |
         |OutOfIterations
         |Final output:
         |""".stripMargin
    )
  }
}
