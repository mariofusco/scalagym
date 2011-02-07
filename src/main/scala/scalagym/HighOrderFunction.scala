package scalagym;

object HighOrderFunction extends Application {

  twice { 
    3 times { counter =>
      println("Hello World - " + counter);
    }
  }

  def twice(block: => Unit): Unit = times(2) { i => block }

  def times(n: Int)(block: Int => Unit): Unit = for (i <- 1 to n) block(i)

  class Looper(n: Int) {
    def times(block: Int => Unit) = HighOrderFunction.times(n)(block)
  }

  implicit def intToLooper(n: Int): Looper = new Looper(n)
}
