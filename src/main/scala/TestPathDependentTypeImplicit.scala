case class MyClass(val value: Int)

/**
  * maintains certain state about MyClass objects' dependencies
  *
  * between below and above are the interesting bits enabling
  * the implicit state awareness leading to a clearer syntax 
  */
class TestPathDependentTypeImplicit(val map: Map[MyClass, MyClass]) {

  // query state
  def depends(one: MyClass, another: MyClass) = {
    map.get(one) match {
      case None        => false
      case Some(value) => value == another
    }
  }

  // ------- below this ----------

  // Extend MyClass with state-aware operations
  class MyClassDependencyAware(myValue: Int) extends MyClass(myValue) {
    def dependsOn(another: MyClass) = depends(this, another)
  }

  // Implicitly convert MyClass to its dependencies-aware version
  implicit def myClassToMyClassDependencyAware(m: MyClass): MyClassDependencyAware = {
    new MyClassDependencyAware(m.value)
  }
  // ------- above this ----------
}

object TestPathDependentTypeImplicit {

  def main(args: Array[String]) {
    val map = Map(
      MyClass(2) -> MyClass(5),
      MyClass(9) -> MyClass(9))
    val t = new TestPathDependentTypeImplicit(map)

    println(t.depends(MyClass(10), MyClass(10)))
    println(t.depends(MyClass(9), MyClass(1)))
    println(t.depends(MyClass(2), MyClass(5)))

    // ------- below this ----------
    // explicitly import the implicit def
    import t._
    // enjoy the implicit awareness of state
    println(MyClass(10) dependsOn MyClass(10))
    println(MyClass(9) dependsOn MyClass(1))
    println(MyClass(2) dependsOn MyClass(5))
    // ------- above this ----------
  }
}
