import scala.collection.immutable.List
import scala.collection.mutable.HashMap

trait RMPart

object InputTypes extends Enumeration {
    type Input = Value

    val Encode, Decode, Run = Value
}

object input {
    val inputType = InputTypes.Run
    val encodeInput: RMPart = RMList(List(2, 1))
    val decodeInput: scala.math.BigInt = 0
    val decodeType =
        DecodeTypes.Instruction // SmallPair, LargePair, List, Program or Instruction

    val runInputProgram = RMProg(
      List(
        ProgDec(1, 1, 2),
        ProgInc(0, 0),
        ProgDec(2, 3, 4),
        ProgInc(0, 2),
        ProgHalt()
      )
    )
    /* HashMap Initialization Syntax: HashMap(0 -> 0, 1 -> 99)
     * is equivalent to: R0 = 0, R1 = 99
     * There are no scratch registers, use a large number instead like R99 = 0
     */
    val runInputParams = HashMap(0 -> 0, 1 -> 1, 2 -> 2)
}
object Main extends App {
    input.inputType match {
        case InputTypes.Decode => {
            println(s"Decode Input received: ${input.decodeInput}\n")
            println(
              s"Decoded Output: \n${Decoder.decode(input.decodeInput, input.decodeType)}"
            )
        }
        case InputTypes.Encode => {
            println("Encode Input Received")
            println(s"Encoded Output: \n${Encoder.encode(input.encodeInput)}")
        }
        case InputTypes.Run => {
            println("Run Input Received")
            println(input.runInputProgram)
            println("Parameters:")

            println(s" L | ${input.runInputParams.keys.mkString(" | ")} |")
            Runner.run(input.runInputProgram, input.runInputParams)
        }
    }
}

object Runner {
    def regToString(instrIdx: Int, map: HashMap[Int, Int]) = {
        print(s" $instrIdx |")
        map.foreach { case (r, v) => print(s" $v |") }
        println()
    }

    def run(program: RMProg, params: HashMap[Int, Int]): Unit = {
        var tracker = 0
        val instrs = program.prog
        var currIdx = 0
        val numInstrs = instrs.length

        regToString(currIdx, params)
        while (instrs(currIdx) != ProgHalt()) {

            if (tracker > 100) {
                println(
                  "Program has hit 100 ran lines. Terminating..."
                )
                return
            }
            instrs(currIdx) match {
                case ProgDec(reg, next, zero) => {
                    params.get(reg) match {
                        case None => {
                            println(
                              s"Register $reg does not exist in provided parameters"
                            )
                            return
                        }
                        case Some(regVal) => {
                            var dest = next
                            if (regVal == 0) {
                                dest = zero
                            } else {
                                params.update(reg, regVal - 1)
                            }
                            if (dest < numInstrs) {
                                currIdx = dest
                            } else {
                                println(
                                  s"Error in instruction - L$currIdx: ${instrs(currIdx)}"
                                )
                                println(
                                  s"Attempt to jump to non-existent instruction L$dest"
                                )
                                return
                            }
                        }
                    }
                }
                case ProgInc(reg, next) => {
                    params.get(reg) match {
                        case None => {
                            println(
                              s"Register $reg does not exist in provided parameters"
                            )
                            return
                        }
                        case Some(regVal) => {
                            params.update(reg, regVal + 1)
                            if (next < numInstrs) {
                                currIdx = next
                            } else {
                                println(
                                  s"Error in instruction - L$currIdx: ${instrs(currIdx)}"
                                )
                                println(
                                  s"Attempt to jump to non-existent instruction L$next"
                                )
                                return
                            }
                        }
                    }
                }
                case ProgHalt() =>
            }

            regToString(currIdx, params)
            tracker += 1
        }

    }
}
