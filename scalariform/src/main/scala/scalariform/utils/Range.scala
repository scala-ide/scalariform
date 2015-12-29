package scalariform.utils

case class Range(offset: Int, length: Int) {

  def contains(other: Range) = other.offset >= offset && other.offset + other.length <= offset + length

  def strictlyContains(other: Range) = (this contains other) && this.length > other.length

  /**
   * @return the smallest range that contains both this and other
   */
  def mergeWith(other: Range) = {
    val List(earliest, latest) = List(this, other) sortBy (_.offset)
    Range(earliest.offset, latest.offset - earliest.offset + latest.length)
  }

  def intersects(other: Range) =
    !(other.offset >= offset + length || other.offset + other.length - 1 < offset)

  def expandLeft(n: Int): Range = Range(offset - n, length + n)

}
