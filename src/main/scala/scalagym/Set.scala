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

trait AbstractSet[A <: Ordered[A]] {
  def contains(x: A): Boolean
  def notContains(x: A) = !contains(x)
  def add(x: A): AbstractSet[A]
}

class EmptySet[A <: Ordered[A]] extends AbstractSet[A] {
  def contains(x: A): Boolean = false
  def add(x: A): AbstractSet[A] = new NonEmptySet(x, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](elem: A, left: AbstractSet[A], right: AbstractSet[A]) extends AbstractSet[A] {
  def contains(x: A): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def add(x: A): AbstractSet[A] =
    if (x < elem) new NonEmptySet(elem, left add x, right)
    else if (x > elem) new NonEmptySet(elem, left, right add x)
    else this
}