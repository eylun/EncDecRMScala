sealed trait Instruction extends RMPart

final case class RMProg(val prog: List[Instruction]) extends RMPart {
    override def toString(): String = {
        val sb = new StringBuilder
        for (idx <- 0 to prog.length - 1) {
            sb ++= s"L$idx: ${prog(idx).toString()}\n"
        }
        sb.toString()
    }
}

case class ProgInc(reg: Int, next: Int) extends Instruction {
    override def toString(): String = s"R$reg+ -> L$next"
}

case class ProgDec(reg: Int, next: Int, zero: Int) extends Instruction {
    override def toString(): String = s"R$reg- -> L$next, L$zero"
}

case class ProgHalt() extends Instruction {
    override def toString(): String = s"HALT"
}
