package vlfig.pilot

abstract class Stack[+A] {
  def push[B >: A](a: B): Stack[B] = NonEmptyStack(a, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}

object EmptyStack extends Stack[Nothing] {
  def isEmpty = true
  def top = sys.error("EmptyStack.top")
  def pop = sys.error("EmptyStack.pop")
}

case class NonEmptyStack[+A](e: A, rest: Stack[A]) extends Stack[A] {
  def isEmpty = false
  def top = e
  def pop = rest
}
