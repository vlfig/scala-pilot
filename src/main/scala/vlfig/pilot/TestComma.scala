package vlfig.pilot
import org.jgrapht.demo.JGraphAdapterDemo

object TestComma extends App {
  val p1 = Person("Lara", false)
  val p2 = Person("John", true, p1)
  val p3 = Person("Ruperth", true)
  val p4 = Person("Susie", false)
  val p5 = Person("Claire", false, p3, p4)
  val persons = List(p1, p2, p3, p4, p5)
  val motherChildPairs = for (p <- persons; if !p.isMale; c <- p.children) yield (p.name, c.name)
  println(motherChildPairs)
  
  
  val zae: JGraphAdapterDemo = new JGraphAdapterDemo  
}
