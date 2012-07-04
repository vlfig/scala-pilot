package vlfig.pilot

abstract class IntTree
case object EmptyTree extends IntTree
case class Node(elem: Int, right: IntTree, left: IntTree) extends IntTree

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

object Main extends App {
  val x = new EmptyStack[Int]
  val y = x.push(1).push(2)
  println(x.push(3).top)
  println(y.pop.top)

  def contains(t: IntTree, v: Int): Boolean = t match {
    case EmptyTree => false
    case Node(e, l, r) => e == v || contains(l, v) || contains(r, v)
  }
  def insert(t: IntTree, v: Int): IntTree = t match {
    case EmptyTree => Node(v, EmptyTree, EmptyTree)
    case Node(e, l, r) => if (e > v) Node(e, l, insert(r, v)) else Node(e, insert(l, v), r)
  }
}
