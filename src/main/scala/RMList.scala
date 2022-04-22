import scala.collection.immutable.List

/* Lists of the form [x1, x2, x3, ...] */
final case class RMList(val elements: List[scala.math.BigInt]) extends RMPart {
    override def toString(): String = s"[${elements.mkString(", ")}]"
}
