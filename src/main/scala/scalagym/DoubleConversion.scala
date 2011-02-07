package scalagym

/**
 * @author Mario Fusco
 */

object DoubleConversion extends Application {

  implicit def stringToInt(s: String) = s.toInt

  implicit def intToRational[T <% Int](i: T) = new Rational(i)

  println("2" add new Rational(1, 2))
}

class Rational(n: Int, d: Int) {

  val num = n
  val den = d

  def this(n: Int) = this(n, 1)

  def add(that: Rational): Rational =
      new Rational(num * that.den + that.num * den, den * that.den)

  override def toString = "" + num + "/" + den
}
