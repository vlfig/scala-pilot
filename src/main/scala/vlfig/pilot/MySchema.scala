package vlfig.pilot
import org.joda.time.DateTime

abstract class MySchema {
  def toXml(): scala.xml.NodeBuffer
}
case class Header(from: String, to: String) extends MySchema {
  def toXml() = {
    <from>{ from }</from><to>{ to }</to>
  }
}
case class Body(what: String, how: Int, when: DateTime, why: Boolean) extends MySchema {
  def toXml() = <what why="{why}">{ what }</what><how when="{when}">{ how }</how>
}
//case class Root(header: Header, body: Body) extends MySchema {
//  def toXml() = <my-schema><head>{ header.toXml().map(_) }</head><body>{ body.toXml().map(_) }</body></my-schema>
//}

object TestXml extends App {
  //  val doc = Root(Header("me", "you"), Body("something", 4, DateTime.now(), true))
  //  println(doc.toXml.text)
}
