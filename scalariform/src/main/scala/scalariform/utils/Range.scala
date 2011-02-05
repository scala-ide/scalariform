package scalariform.utils

case class Range(offset: Int, length: Int) { 

  def contains(range: Range) = range.offset >= offset && range.offset + range.length <= offset + length

}
