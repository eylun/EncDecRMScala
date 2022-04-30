object DecodeTypes extends Enumeration {
    type DecodeType = Value

    val SmallPair, LargePair, List, Program, Instruction = Value
}

object Decoder {
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
