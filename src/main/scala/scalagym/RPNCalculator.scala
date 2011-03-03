package scalagym

/**
 * @author Mario Fusco
 */

object RPNCalculator extends Application {

  def calc(s: String): Int = calc(s.split(" ").toList)

  def calc(ss: List[String]): Int = (List[Int]() /: ss)(foldFunction).head

  implicit def toProcessor[A](a: A) = new {
    def |>[B](f: PartialFunction[A, B]): B = f(a)
  }

  def foldFunction(ints: List[Int], s: String): List[Int] = {
    println("folding " + ints + " with " + s)
    s |> {
      doOperation(ints) orElse { case _ => s.toInt :: ints }
    }
  }

  implicit def toFolder[A](list: List[A]) = new {
    def fold2(op: (A, A) => A) = list match { case s1 :: s2 :: tail => op(s2, s1) :: tail }
  }

  def doOperation[A](ints: List[Int]): PartialFunction[String, List[Int]] = {
    case "+" => ints fold2 (_ + _)
    case "-" => ints fold2 (_ - _)
    case "*" => ints fold2 (_ * _)
  }

  println(calc("10 4 3 + 2 * -"))
}