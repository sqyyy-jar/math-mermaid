import scala.collection.mutable.ArrayBuffer

case class Node[T](edge: Fraction, value: T, leafs: Seq[Node[T]])

object Node:
  def from[T](
      quantities: Quantities[T],
      depth: Int,
      edge: Fraction,
      value: T
  ): Node[T] =
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
    Node(edge, value, leafs)

  def fromNoPutBack[T](elements: Seq[T], depth: Int): Node[T] =
    todo
