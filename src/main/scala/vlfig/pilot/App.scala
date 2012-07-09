package vlfig.pilot

abstract class Stack[A] {
  def push(a: A): Stack[A] = NonEmptyStack(a, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}

case class EmptyStack[A] extends Stack[A] {
  def isEmpty = true
  def top = sys.error("EmptyStack.top")
  def pop = sys.error("EmptyStack.pop")
}

case class NonEmptyStack[A](e: A, rest: Stack[A]) extends Stack[A] {
  def isEmpty = false
  def top = e
  def pop = rest
}


trait Set[A] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

case class EmptySet[A <: Ordered[A]] extends Set[A] {
  def contains(x: A): Boolean = false
  def incl(x: A): Set[A] = new NonEmptySet[A](x, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {

  def contains(x: A): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: A): Set[A] =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this
}


abstract class Tree[A] {
  //  def contains(t: Tree[A], v: A): Boolean = t match {
  //    case EmptyTree => false
  //    case Node(e, l, r) => e == v || contains(l, v) || contains(r, v)
  //  }
  //  def insert(t: Tree[A], v: A): Tree[A] = t match {
  //    case EmptyTree => Node(v, EmptyTree, EmptyTree)
  //    case Node(e, l, r) => if (e > v) Node(e, l, insert(r, v)) else Node(e, insert(l, v), r)
  //  }
}
case class EmptyTree extends Tree
case class Node[A](elem: A, right: Tree[A], left: Tree[A]) extends Tree[A]

object Main extends App {
  val x = new EmptyStack[Int]
  val y = x.push(1).push(2)
  println(x.push(3).top)
  println(y.pop.top)

}
