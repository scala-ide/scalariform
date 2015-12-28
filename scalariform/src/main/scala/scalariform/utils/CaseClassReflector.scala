package scalariform.utils

trait CaseClassReflector extends Product {

  def getFields: List[(String, Any)] = {
    val names = getClass.getDeclaredFields map { _.getName }
    names.toList zip productIterator.toList
  }

}
