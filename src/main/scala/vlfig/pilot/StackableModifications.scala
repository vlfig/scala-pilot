package vlfig.pilot

import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int
  def put(i: Int)
}

class BasicIntQueue extends IntQueue {
  private val buffer = new ArrayBuffer[Int]
  def get() = { buffer.remove(0) }
  def put(i: Int) = { buffer += i }
}

trait Doubling extends IntQueue {
  abstract override def get() = { super.get() * 2 }
}

object DoublingQueue extends BasicIntQueue with Doubling

class StackableModifications {
  val dq = new BasicIntQueue with Doubling
}