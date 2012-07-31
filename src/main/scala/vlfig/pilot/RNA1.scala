package vlfig.pilot
import scala.collection.IndexedSeqLike
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer

abstract class Base
case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base
object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
}

final class RNA1 private (val groups: Array[Int], val length: Int) extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA1] {

  import RNA1._

  override def newBuilder: Builder[Base, RNA1] =
    new ArrayBuffer[Base] mapResult fromSeq

  def apply(idx: Int): Base = {
    if (idx < 0 || idx >= length)
      throw new IndexOutOfBoundsException

    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }

}

object RNA1 {
  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA1 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA1(groups, buf.length)
  }

  def apply(bases: Base*) = {
    RNA1.fromSeq(bases)
  }
}

object Test extends App {
  println(1 << 2)
  println((1 << 2) - 1)

  val xs = List(A, T, G, A, U, T)
  val r1 = RNA1.fromSeq(xs)
  val r2 = RNA1(A, G, A, T, U, T, G, A, G)
  val r3 = RNA1(A, T, G, A, U, T)
  println(xs)
  println(r1)
  println(r2)
  println(xs take 3)
  println(xs drop 3)
  println(r1.head)
  println(r1.tail)
  println(r1 take 2)
  println(r2 filter (_ == T))

}

