package scalagym

/**
 * @author Mario Fusco
 */

object DoubleConversion extends Application {

  implicit def stringToInt(s: String) = s.toInt

  implicit def intToRational[T <% Int](i: T) = new RationalNumber(i)

  println("2" add new RationalNumber(1, 2))
}

class RationalNumber(n: Int, d: Int) {

  val num = n
  val den = d

  def this(n: Int) = this(n, 1)

  def add(that: RationalNumber): RationalNumber =
      new RationalNumber(num * that.den + that.num * den, den * that.den)

  override def toString = "" + num + "/" + den
}
