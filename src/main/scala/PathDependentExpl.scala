class Outer(val times: Int) {

  class Inner(val count: Int)
  
  object Inner {
    def apply(i: Int) = new Inner(i)
  }
  
  def method(i: Inner) = i.count * times
}

class PathDependentExpl {
  val o = new Outer(3)
  val i = o.Inner(2)
  val p = new Outer(5)
  val j = p.Inner(3)
  
  println(o.method(i))
  println(p.method(j))
//  println(p.method(i)) // these two mistakes 
//  println(o.method(j)) // don't even compile
}
