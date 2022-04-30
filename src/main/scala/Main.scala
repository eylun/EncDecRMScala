import scala.collection.immutable.List

trait RMPart

object encoder {
    def encode(dec: RMPart): scala.math.BigInt = dec match {
        case LPair(left, right)   => encodeLarge(left, right)
        case SPair(left, right)   => encodeSmall(left, right)
        case RMList(elements)     => encodeList(elements)
        case RMProg(instructions) => encodeProg(instructions)
        case i: Instruction       => encodeInstr(i)
    }

    def encodeLarge(
        left: scala.math.BigInt,
        right: scala.math.BigInt
    ): scala.math.BigInt = {
        println("Encoding large pair...")
        println(LPair(left, right).toString)
        val result =
            (scala.math.pow(2, left.toInt).toLong * (2 * right + 1)) - 1
        println(s"= 2^$left * (2($right) + 1) - 1")
        println(s"= ${scala.math.pow(2, left.toInt)} * ${2 * right + 1} - 1")
        println(s"= $result")
        result
    }

    def encodeSmall(
        left: scala.math.BigInt,
        right: scala.math.BigInt
    ): scala.math.BigInt = {
        println("Encoding small pair...")
        println(SPair(left, right).toString)
        val result = (scala.math.pow(2, left.toInt).toLong * (2 * right + 1))
        println(s"= 2^$left * (2($right) + 1)")
        println(s"= ${scala.math.pow(2, left.toInt)} * ${2 * right + 1}")
        println(s"= $result")
        result
    }

    def encodeList(elements: List[scala.math.BigInt]): scala.math.BigInt = {
        println("Encoding List of Numbers...")
        println(RMList(elements).toString())
        val result = elements.foldRight(scala.math.BigInt(0))(encodeSmall)
        println(s"= $result")
        result
    }

    def encodeProg(instructions: List[Instruction]): scala.math.BigInt = {
        println("Encoding program...")
        println(RMProg(instructions).toString())
        val result = encodeList(
          instructions.map(encodeInstr)
        )
        println(s"= $result")
        result
    }

    def encodeInstr(instr: Instruction): scala.math.BigInt = instr match {
        case ProgDec(reg, next, zero) => {
            println(s"Encoding R$reg- -> L$next, L$zero")
            val result = encodeSmall(2 * reg + 1, encodeLarge(next, zero))
            println(s"= $result")
            result
        }
        case ProgInc(reg, next) => {
            println(s"Encoding R$reg+ -> L$next")
            val result = encodeSmall(2 * reg, next)
            println(s"= $result")
            result
        }
        case ProgHalt() => {
            println("Encoding HALT = 0")
            0.toLong
        }
    }
}

object DecodeTypes extends Enumeration {
    type DecodeType = Value

    val SmallPair, LargePair, List, Program, Instruction = Value
}

object decoder {
    def decode(num: scala.math.BigInt, t: DecodeTypes.DecodeType): RMPart =
        t match {
            case DecodeTypes.SmallPair   => decodeSmall(num)
            case DecodeTypes.LargePair   => decodeLarge(num)
            case DecodeTypes.List        => decodeList(num)
            case DecodeTypes.Program     => decodeProg(num)
            case DecodeTypes.Instruction => decodeInstr(num)
        }

    def decodeSmall(num: scala.math.BigInt) = {
        println("Decoding small pair...")
        var n = num
        var left = 0
        while (n % 2 == 0 && n != 0) {
            left += 1
            n = n / 2
        }
        val right = (n - 1) / 2
        println(
          s"$num = ${scala.math.pow(2, left).toLong} * ${2 * right + 1}"
        )
        println(
          s"$num = 2^$left * (2($right) + 1)"
        )
        val result = SPair(left, right)
        println(result.toString())
        result
    }
    def decodeLarge(num: scala.math.BigInt) = {
        println("Decoding large pair...")
        var n = num + 1
        var left = 0
        while (n % 2 == 0 && n != 0) {
            left += 1
            n = n / 2
        }
        val right = (n - 1) / 2
        println(
          s"$num + 1 = ${scala.math.pow(2, left).toLong} * ${2 * right + 1}"
        )
        println(s"$num = 2^$left * (2($right) + 1) - 1")
        val result = LPair(left, right)
        println(result.toString())
        result
    }
    def decodeList(num: scala.math.BigInt): RMList = {
        println("Decoding List...")
        import scala.collection.mutable.ListBuffer
        val lb: ListBuffer[scala.math.BigInt] = ListBuffer()
        var SPair(left: scala.math.BigInt, right: scala.math.BigInt) =
            decodeSmall(num)
        while (right != 0) {
            lb += left
            val spair = decodeSmall(right)
            left = spair.l
            right = spair.r
        }
        lb += left
        val result = RMList(lb.toList)
        println(result.toString())
        result
    }
    def decodeProg(num: scala.math.BigInt): RMProg = {
        println("Decoding Program...")
        val r @ RMList(instrList) = decodeList(num)
        println(s"Program List forms ${r.toString}")
        val result = RMProg(instrList.map(decodeInstr))
        println(result.toString())
        result
    }

    def decodeInstr(num: scala.math.BigInt): Instruction = {
        println("Decoding Instruction...")
        num match {
            case _ if num == 0 => {
                println("HALT")
                ProgHalt()
            }
            case _ => {
                val SPair(left, right) = decodeSmall(num)
                if (left % 2 == 0) { // even - This is a inc instruction
                    println(s"R${left / 2} -> L$right")
                    ProgInc((left / 2).toInt, right.toInt)
                } else { // odd - This is a dec instruction
                    val LPair(smallLeft, smallRight) = decodeLarge(right)
                    println(s"R${left / 2} -> L$smallLeft, L$smallRight")
                    ProgDec((left / 2).toInt, smallLeft.toInt, smallRight.toInt)
                }
            }
        }
    }
}

object input {
    val encodeInput: Option[RMPart] = Some(RMList(List(2, 1)))
    val decodeInput: Option[scala.math.BigInt] = None
    val decodeType =
        DecodeTypes.Instruction // SmallPair, LargePair, List, Program or Instruction
}
object Main extends App {
    (input.decodeInput, input.encodeInput) match {
        case (None, None) => println("No provided Input, please provide 1.")
        case (Some(_), Some(_)) =>
            println("Provided 2 inputs, please only provide 1.")
        case (Some(dec), None) => {
            println(s"Decode Input received: ${dec}\n")
            println(
              s"Decoded Output: \n${decoder.decode(dec, input.decodeType)}"
            )
        }
        case (None, Some(enc)) => {
            println("Encode Input Received")
            println(s"Encoded Output: \n${encoder.encode(enc)}")
        }
    }
}
