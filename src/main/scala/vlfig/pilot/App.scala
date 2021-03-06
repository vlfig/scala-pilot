package vlfig.pilot

import scala.annotation.tailrec

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

case class Person(name: String, isMale: Boolean, children: Person*)

object Main extends App {
  val x = EmptyStack
  val y = x.push(1).push(2).push("Ola").push("ole")
  println(x.push(3).top + 2)
  println(y.top)

  val s = new EmptySet[Num].incl(Num(1.0)).incl(Num(2.0))
  println(s.contains(Num(1.5)))

  val inc: (Int => Int) = x => x + 1
  println(inc(2))

  val l = List[String]()
  println(l :: Nil tail)

  def isort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: xs => insert(x, isort(xs))
  }
  def insert(e: Int, l: List[Int]): List[Int] = l match {
    case List() => List(e)
    case x :: xs => if (e <= x) e :: x :: xs else x :: insert(e, xs)
  }
  def length(l: List[AnyVal]): Int = l match {
    case Nil => 0
    case x :: xs => recLength(1, xs)
  }
  //  def crap[A,B](f: A => B)(l: List[A]): List[B] = l match {
  //    case Nil => Nil
  //    case x::xs => f(x) :: crap(f)(xs)
  //  }

  def crap[A <: AnyVal, B](f: A => B)(l: List[A]): List[B] = l match {
    case Nil => Nil
    case x :: xs => f(x) :: crap(f)(xs)
  }

  @tailrec
  def recLength(acum: Int, l: List[AnyVal]): Int = l match {
    case Nil => acum
    case x :: xs => recLength(acum + 1, xs)
  }

  val m = 1 :: 3 :: 2 :: 4 :: 7 :: 5 :: Nil
  val n = 6 :: 8 :: 10 :: 9 :: Nil

  println(m)
  println(isort(m))
  println(length(m))
  println(n)
  println(isort(n))
  println(length(n))

  val o = m ::: n
  val p = m ::: n

  println(o)
  println(m ::: n filter (x => x > 2))
  val total = (1 /: m) { (x, y) => x + y }
  val reverse = ((Nil: List[Int]) /: o) { (xs, x) => x :: xs }
  println(reverse)
  println("total is " + total)
  println(isort(o) map (x => x * x))
  println(crap[Int, Double](z => z * 2.71828183)(m ::: n))
  println(length(o))

  val k = 5

  println(List.range(1, k))
  println(List.range(1, k).map(i => List.range(1, i) map (x => (i, x))))
  println(List.range(1, k).map(i => List.range(1, i) map (x => (i, x)))
    .foldRight(List[(Int, Int)]()) { (xs, ys) => xs ::: ys })
  println(List.range(1, k).flatMap(i => List.range(1, i) map (x => (i, x))))
  println(List.range(1, k).map(i => List.range(1, i) map (x => (i, x)))
    .foldRight(List[(Int, Int)]()) { (xs, ys) => xs ::: ys }
    .filter(pair => (pair._1 + pair._2) % 5 == 0))

  val greets = "ola" :: "ole" :: "hi" :: "season greetings" :: Nil
  println("above 3: " + greets.count(_.length > 3))
  println(greets.mkString(", "))
  val numberedGreetings = for {
    str <- greets
    num <- o if str.startsWith("o") && num < 6
  } yield num + " " + str
  println(numberedGreetings.size)
  numberedGreetings.foreach(println)

  val nouns = "dude" :: "dad" :: "mom" :: "moron" :: Nil

  import scala.collection.immutable.TreeSet
  val sortedNouns = TreeSet[String]() ++ nouns

  val anotherGreetSeq =
    for (
      (zemanel, cajo) <- greets zip sortedNouns
    ) yield zemanel + ", " + cajo
  println((anotherGreetSeq mkString ";\n") + ".")
  println(greets.head)
  println(greets.tail)
  println(greets.init)
  println(greets.last)

  val p1 = Person("Lara", false)
  val p2 = Person("John", true, p1)
  val p3 = Person("Ruperth", true)
  val p4 = Person("Susie", false)
  val p5 = Person("Claire", false, p3, p4)
  val persons = List(p1, p2, p3, p4, p5)
  val motherChildPairs = for (p <- persons if !p.isMale; c <- p.children) yield (p.name, c.name)
  println(motherChildPairs)
}

