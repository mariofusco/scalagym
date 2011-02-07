package scalagym

/**
 * @author Mario Fusco
 */

object Curry extends Application {
/*
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }

  val sum1To10 = sum(x => x)(1, 10)
*/


  def sum(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sum(f)(a + 1, b)

  val sum1To10 = sum(x => x)(1, 10)


/*
  def sum(a: Int, b: Int)(f: Int => Int): Int = if (a > b) 0 else f(a) + sum(a + 1, b)(f)

  val sum1To10 = sum(1, 10) { x => x }
*/
  println(sum1To10)
}