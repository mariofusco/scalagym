package scalagym

/**
 * @author Mario Fusco
 */

class Rational(n: Int, d: Int) {

  val num = n
  val den = d

  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational =
      new Rational(num * that.den + that.num * den, den * that.den)

  def + (i: Int): Rational = new Rational(num + i * den, den)

  override def toString = "" + num + "/" + den
}

object RationalOne extends Rational(1)

object Rational {
  def apply(n: Int) = new Rational(n)
  def apply(n: Int, d: Int) = new Rational(n, d)
}

object Runner extends Application {
  val one = RationalOne
  val two = Rational(1) + one
  println(two)
}
