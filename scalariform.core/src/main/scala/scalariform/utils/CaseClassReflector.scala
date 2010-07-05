package scalariform.utils

trait CaseClassReflector extends Product {

  def getFields: List[(String, Any)] = {
    val names = getClass.getDeclaredFields map { _.getName }
    names.toList zip productIterator.toList
  }

  private def getFieldsOld: List[(String, Any)] = {
    var fieldValueToName: Map[Any, String] = Map()
    for (field ← getClass.getDeclaredFields) {
      field.setAccessible(true)
      fieldValueToName += (field.get(this) -> field.getName)
    }
    productIterator.toList map { value ⇒ fieldValueToName(value) -> value }
  }

}
