package scalagym

/**
 * @author Mario Fusco
 */

/*
trait Set[+A] {
  def contains[B >: A](x: B): Boolean
  def add[B >: A](x: B): Set[B]
}
*/

trait Set[A <: Ordered[A]] {
  def contains(x: A): Boolean
  def notContains(x: A) = !contains(x)
  def add(x: A): Set[A]
}

class EmptySet[A <: Ordered[A]] extends Set[A] {
  def contains(x: A): Boolean = false
  def add(x: A): Set[A] = new NonEmptySet(x, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {
  def contains(x: A): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def add(x: A): Set[A] =
    if (x < elem) new NonEmptySet(elem, left add x, right)
    else if (x > elem) new NonEmptySet(elem, left, right add x)
    else this
}