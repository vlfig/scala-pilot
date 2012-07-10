package vlfig.pilot

trait Set[A] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

case class EmptySet[A <% Ordered[A]] extends Set[A] {
  def contains(x: A): Boolean = false
  def incl(x: A): Set[A] = new NonEmptySet[A](x, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <% Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {

  def contains(x: A): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: A): Set[A] =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this
}
