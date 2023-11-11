import scala.collection.mutable.ArrayBuffer
case class Tree[T](leafs: Seq[Node[T]]):
  def probability(path: Seq[T]): Double =
    todo

object Tree:
  def parse[T](elements: Seq[T], depth: Int): Tree[T] =
    val quantities = Quantities.from(elements)
    val leafs =
      if depth <= 0 then Seq[Node[T]]()
      else
        val leafs = ArrayBuffer[Node[T]]()
        for (value, quantity) <- quantities.items do
          leafs += Node.from(
            quantities,
            depth - 1,
            Fraction(quantity, quantities.total),
            value
          )
        leafs.toSeq
    Tree(leafs)

  def parseNoPutBack[T](elements: Seq[T], depth: Int): Option[Tree[T]] =
    todo
