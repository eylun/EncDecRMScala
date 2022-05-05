object Encoder {
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
        println(s"Encoding List of Numbers: ${elements} OR 0b${elements
            .map(n => { s"$n zeroes" })}")
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
