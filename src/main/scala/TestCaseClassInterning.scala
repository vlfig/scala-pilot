import scala.collection.mutable.WeakHashMap

/**
  * Utility traits for classes whose companion objects are to
  * intern their instances, never creating repeated objects.
  *
  * Instances are cached based on their immutable
  * arguments, as provided to companion object's {{apply()}}.
  */

trait Applyable1[A, Z] {
  def apply(a: A): Z
}
trait Internable1[A, Z] extends Applyable1[A, Z] {
  private[this] val cache = WeakHashMap[(A), Z]()
  private[this] def intern(args: (A))(builder: => Z) = {
    cache.getOrElse(args, {
      val newObj = builder
      cache(args) = newObj
      newObj
    })
  }
  abstract override def apply(arg: A) = {
    println("Internable1: hijacking apply")
    intern(arg) { super.apply(arg) }
  }
}

trait Applyable2[A, B, Z] {
  def apply(a: A, b: B): Z
}
trait Internable2[A, B, Z] extends Applyable2[A, B, Z] {
  private[this] val cache = WeakHashMap[(A, B), Z]()
  private[this] def intern(args: (A, B))(builder: => Z) = {
    cache.getOrElse(args, {
      val newObj = builder
      cache(args) = newObj
      newObj
    })
  }
  abstract override def apply(a: A, b: B) = {
    println("Internable2: hijacking apply")
    intern((a, b)) { super.apply(a, b) }
  }
}

// class with one apply arg 
abstract class SomeClassCompanion extends Applyable1[Int, SomeClass] {
  def apply(value: Int): SomeClass = {
    println("original apply")
    new SomeClass(value)
  }
}
class SomeClass(val value: Int)
object SomeClass extends SomeClassCompanion with Internable1[Int, SomeClass]

// class with two apply arg 
abstract class AnotherClassCompanion extends Applyable2[String, String, AnotherClass] {
  def apply(one: String, two: String): AnotherClass = {
    println("original apply")
    new AnotherClass(one, two)
  }
}
class AnotherClass(val one: String, val two: String)
object AnotherClass extends AnotherClassCompanion with Internable2[String, String, AnotherClass]

/*
scala> import SomeClass._
import SomeClass._

scala> SomeClass(1)
Internable1: hijacking apply
original apply
res0: SomeClass = SomeClass@2e239525

scala> import AnotherClass._
import AnotherClass._

scala> AnotherClass("earthling", "greetings")
Internable2: hijacking apply
original apply
res1: AnotherClass = AnotherClass@329b5c95

scala> AnotherClass("earthling", "greetings")
Internable2: hijacking apply
res2: AnotherClass = AnotherClass@329b5c95
*/
