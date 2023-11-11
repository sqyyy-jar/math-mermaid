case class Fraction(num: Int, den: Int):
  def eval: Double =
    num.toDouble / den.toDouble

  override def toString: String =
    s"$num/$den"
