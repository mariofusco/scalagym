package scalagym

object Combinator extends Application {

  val l1 = List(1, 2, 3, 4)
  val l2 = List("a", "b", "c")
  val l3 = List("I", "II")

  val p = l3 map (List(_)) flatMap (i => l2.map(x => x :: i)) flatMap (i => l1.map(x => x :: i))

  println(p)

  val l = List(List(1, 2, 3, 4), List("a", "b", "c"), List("I", "II"))

  def cartesianProduct[A <: Any](l: List[List[A]]) = (l :\ List(List[A]())) {
    (ys, xss) => xss flatMap (xs => ys map (y => y :: xs))
  }

  println(cartesianProduct(l))

  def cartesianProduct2[A <: Any](l: List[List[A]]) = l.map(_.map(List(_))).reduceLeft((xs, ys) => for {x <- xs; y <- ys} yield x ++ y)

  println(cartesianProduct2(l))
}