package scalagym

import annotation.tailrec

/**
 * @author Mario Fusco
 */

object Fold extends Application {

  val animals = List("dog", "cat", "horse", "rabbit")

//  val sum = animals.foldLeft(0)(_ + _.length)
//  val sum = animals.foldLeft(0)(_ + _.length)

  val sum = (0 /: animals)((s,t) => s + t.length)
  println(sum)

  def myFoldRight[A, B](list: List[A])(start: B, f: (A, B) => B): B = list match {
    case Nil => start
    case x :: xs => f(x, myFoldRight(xs)(start, f))
  }

  val myFoldRightSum = myFoldRight(animals)(0, (s: String, t: Int) => s.length + t)
  println(myFoldRightSum)

  def myFoldLeft[A, B](list: List[A])(start: B, f: (A, B) => B): B = {
    @tailrec
    def loop(acc: B, rem: List[A]): B = rem match {
      case Nil => acc
      case x :: xs => loop(f(x, acc), xs)
    }
    loop(start, list)
  }

  val myFoldLeftSum = myFoldLeft(animals)(0, (s: String, t: Int) => s.length + t)
  println(myFoldLeftSum)

  implicit def folder[A](list: List[A]) = new {
    def aggregate[B](start: B, f: (A, B) => B) = Fold.myFoldLeft(list)(start, f)
  }
//  println(animals.aggregate(0, (s: String, t: Int) => s.length + t))

  def sumStringLength(strings: List[String]) = myFoldLeft(strings)(0, (s: String, t: Int) => s.length + t)
  println(sumStringLength(animals))
}