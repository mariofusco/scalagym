package scalagym

/**
 * @author Mario Fusco
 */

object MonadTest extends Application {

  def stringToInt(string: String) : Option[Int] = try {
      Some(string.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
  }

  def readPositiveIntParam(params: Map[String, String], name: String) =
    params get name flatMap stringToInt filter (_ > 0) getOrElse 0

  def readPositiveIntParam2(params: Map[String, String], name: String) = (for {
      param <- params get name; value <- stringToInt(param) if (value > 0)
  } yield value) getOrElse 0

  val params = Map("a" -> "5", "b" -> "false", "c" -> "-3")

  println(readPositiveIntParam2(params, "a"))
  println(readPositiveIntParam2(params, "b"))
  println(readPositiveIntParam2(params, "c"))
  println(readPositiveIntParam2(params, "d"))
}