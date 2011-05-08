package scalagym;

object Reflection extends Application {

  val l = List(1, "One", true)

  println(filterByClass(l, classOf[String]))
  println(filterByClass(l, classOf[Int]))
  println(filterByClass(l, classOf[Boolean]))

  def filterByClass[A](l: List[_], c: Class[A]) =
    l filter (normalizeClass(c).isInstance(_))

  private def normalizeClass(c: Class[_]): Class[_] = {
    if (classOf[AnyRef].isAssignableFrom((c))) c
    else if (c == classOf[Int]) classOf[java.lang.Integer]
    else if (c == classOf[Long]) classOf[java.lang.Long]
    else if (c == classOf[Double]) classOf[java.lang.Double]
    else if (c == classOf[Float]) classOf[java.lang.Float]
    else classOf[java.lang.Boolean]
  }
}