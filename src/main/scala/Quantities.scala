import scala.collection.mutable.ArrayBuffer

case class Quantities[T](total: Int, items: Seq[(T, Int)])

object Quantities:
  def from[T](elements: Seq[T]): Quantities[T] =
    val quantities = ArrayBuffer[(T, Int)]()
    for element <- elements do
      val index = quantities.indexWhere { (value, _) => value == element }
      if index != -1 then
        val (value, i) = quantities(index)
        quantities(index) = (value, i + 1)
      else quantities += ((element, 1))
    Quantities(elements.length, quantities.toSeq)
