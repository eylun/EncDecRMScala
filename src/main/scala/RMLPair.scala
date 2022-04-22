/* Large Pairs of the form <left, right> */
final case class LPair(
    val left: scala.math.BigInt,
    val right: scala.math.BigInt
) extends RMPart {
    override def toString(): String = s"<$left, $right>"
}
