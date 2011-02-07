package scalariform.utils

import scala.math.Ordering

case class Range(offset: Int, length: Int) {

  def contains(other: Range) = other.offset >= offset && other.offset + other.length <= offset + length

  def mergeWith(other: Range) = {
    val List(earliest, latest) = List(this, other) sortBy (_.offset)
    Range(earliest.offset, latest.offset - earliest.offset + latest.length)
  }

  def isLargerThan(other: Range) = length > other.length

}
