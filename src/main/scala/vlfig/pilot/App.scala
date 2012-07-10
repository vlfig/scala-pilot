package vlfig.pilot

case class Num(value: Double) extends Ordered[Num] {
  def compare(that: Num): Int =
    if (this.value < that.value) -1
    else if (this.value > that.value) 1
    else 0
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
  val x = EmptyStack
  val y = x.push(1).push(2).push("Ola").push("ole")
  println(x.push(3).top+2)
  println(y.top)

  val s = new EmptySet[Num].incl(Num(1.0)).incl(Num(2.0))
  println(s.contains(Num(1.5)))
}
