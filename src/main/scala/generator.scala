import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.boundary

// - Find total counts of input variants
// - Create an ArrayBuffer that will store the indices of the variants of the path
// - Call generateSubtree recursively

private type Counts[T] = ArrayBuffer[(T, Int)]

private class State[T](
    val inputCount: Int,
    val counts: Counts[T],
    val out: StringBuilder = StringBuilder(),
    val path: ArrayBuffer[Int] = ArrayBuffer(),
    val usePath: Boolean = true,
    var probability: Fraction = Fraction(1, 1),
    var id: Int = 0
):
  private def allocId =
    val nId = id
    id += 1
    nId

  def genStart: Int =
    out.append(s"""flowchart LR
                  |  n$id((\" \"))""".stripMargin)
    allocId

  private def genNode(value: T): Int =
    out.append(s"""
                  |  n$id(("$value"))""".stripMargin)
    allocId

  private def genPath(startId: Int, endId: Int, probability: Fraction) =
    out.append(s"""
                  |  n$startId--"$probability"---n$endId""".stripMargin)

  private def genEndPath(id: Int, probability: Fraction) =
    val nId = allocId
    out.append(f"""
                  |  n$nId("${probability.eval * 100.0}%.2f%%")
                  |  n$id-.-n$nId""".stripMargin)

  def genSubtree(depth: Int, parentId: Int): Unit =
    if depth == 0 then
      genEndPath(parentId, probability)
      return ()
    // Go through all variants and
    // - check if the variant was used too many times
    // - if not, continue
    // - update path
    // - update probability
    val elementCount = elementsLeft
    for i <- counts.indices do
      val (value, count) = counts(i)
      val pathCount = path.count { _ == i }
      if pathCount < count then
        if usePath then path.append(i) // update path
        val oldProb = probability // update probability
        probability = Fraction(
          oldProb.num * (count - pathCount),
          oldProb.den * elementCount
        ) // P(parent -> this)
        val nId = genNode(value)
        genPath(parentId, nId, probability) // generate parent -> this
        genSubtree(depth - 1, nId) // generate this -> next
        probability = oldProb // restore probability
        if usePath then path.remove(path.length - 1) // restore path

  def elementsLeft: Int =
    var elements = inputCount
    for i <- counts.indices do
      val pathCount = path.count { _ == i }
      elements -= pathCount
    elements

def genTree[T](depth: Int, inputs: Array[T], usePath: Boolean): String =
  val inputCount = inputs.length
  val variantCounts = findCounts(inputs)
  val state = State(inputCount, variantCounts, usePath = usePath)
  state.genStart
  state.genSubtree(depth, 0)
  state.out.toString()

private def findCounts[T](inputs: Array[T]): Counts[T] =
  val counts = ArrayBuffer[(T, Int)]()
  for input <- inputs do
    val index = counts.indexWhere { (value, _) => value == input }
    if index != -1 then
      val (value, i) = counts(index)
      counts(index) = (value, i + 1)
    else counts += ((input, 1))
  counts
