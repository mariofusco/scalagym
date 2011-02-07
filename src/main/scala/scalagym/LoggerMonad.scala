package scalagym

/**
 * @author Mario Fusco
 */

case class LoggerMonad[A](log: List[String], value: A) {
  def map[B](f: A => B) = LoggerMonad(log, f(value))

  def flatMap[B](f: A => LoggerMonad[B]) = {
    val x = f(value)
    LoggerMonad(log ::: x.log, x.value)
  }
}

object LoggerMonad extends Application {

  implicit def PreLoggerUtil(s: String) = new { def |->[A](a: A) = LoggerMonad(List(s), a) }

  implicit def PostLoggerUtil[A](a: A) = new { def loggedAs(s: String) = LoggerMonad(List(s), a) }

  val r =
    for (
        a <- addOne(123);
        b <- addTwo(a);
        c <- addOne(b);
        d <- addTwo(c)
       ) yield d

  println("RESULT: " + r.value)
  r.log foreach println

  def addOne(n: Int) = ("adding one to " + n) |-> (n + 1)

  def addTwo(n: Int) =  (n + 2) loggedAs ("adding two to " + n)
}

