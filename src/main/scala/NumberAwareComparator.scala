import java.util.Comparator

import scala.annotation.tailrec

class NumberAwareComparator extends Comparator[String] {
  override def compare(lhs: String, rhs: String): Int =
    comparePairs(tokenSequence(lhs).zip(tokenSequence(rhs)))

  @tailrec private def comparePairs(pairs: Stream[(ComparableUnit, ComparableUnit)]): Int = pairs match {
    case (lhs, rhs) #:: tail =>
      val result = lhs.compareTo(rhs)
      if (result != 0 || tail.isEmpty) result else comparePairs(tail)
  }

  private def tokenSequence(string: String): Stream[ComparableUnit] =
    ComparableUnitTokenizer.stream(string) ++ Some(EmptyUnit).toStream
}

object ComparableUnitTokenizer {
  def nextToken(string: String): (ComparableUnit, String) =
    if (string.isEmpty) throw new IllegalStateException("string is empty")
    else if (string.head.isDigit) {
      val end = string.indexWhere(ch => !ch.isDigit)
      val (number, tail) = if (end == -1) (string, "") else string.splitAt(end)
      (NumberUnit(number.toInt), tail)
    } else (CharUnit(string.head), string.tail)

  def stream(string: String): Stream[ComparableUnit] = nextToken(string) match {
    case (token, tail) => token #:: (if (tail.isEmpty) Stream.empty else stream(tail))
  }
}

trait ComparableUnit {
  def compareTo(other: ComparableUnit): Int
  def asString(): String
}

object EmptyUnit extends ComparableUnit {
  override def compareTo(other: ComparableUnit): Int = other match {
    case EmptyUnit => 0
    case _ => -1
  }

  override def asString(): String = ""
}

case class CharUnit(value: Char) extends ComparableUnit {
  override def compareTo(other: ComparableUnit): Int =
    value.toString.compareTo(other.asString())

  override def asString(): String = value.toString
}

case class NumberUnit(value: Int) extends ComparableUnit {
  override def compareTo(other: ComparableUnit): Int = other match {
    case NumberUnit(otherValue) => value.compareTo(otherValue)
    case _ => this.asString().compareTo(other.toString)
  }

  override def asString(): String = value.toString
}
