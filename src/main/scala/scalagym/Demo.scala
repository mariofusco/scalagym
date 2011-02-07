package scalagym

import annotation.tailrec

/**
 * @author Mario Fusco
 */
object Demo extends Application {

  implicit def intToLooper(n: Int) = new { def times(f: => Unit): Unit = Demo.times(n)(f) }

  twice {
    3 times {
      println("Hello world!")
    }
  }

  def twice(f: => Unit): Unit = { f; f; }

  @tailrec
  def times(n: Int)(f: => Unit): Unit = if (n > 0) { f; times(n-1)(f) }

}