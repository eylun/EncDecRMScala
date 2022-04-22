/* Small Pairs of the form <<left, right>> */
final case class SPair(
    val left: scala.math.BigInt,
    val right: scala.math.BigInt
) extends RMPart {
    override def toString(): String = s"<<$left, $right>>"

    val l = left

    val r = right
}
