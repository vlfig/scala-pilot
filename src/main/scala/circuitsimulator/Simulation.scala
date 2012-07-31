package circuitsimulator

class Simulation {

  type Action = () => Unit

  case class WorkItem(time: Int, action: () => Unit)

  private var curTime = 0
  def currentTime: Int = curTime

  var agenda: List[WorkItem] = List()

  private def insert (ag: List[WorkItem], i: WorkItem): List[WorkItem] = {
    if(ag.isEmpty || i.time < ag.head.time) i :: ag
    else ag.head :: insert(ag.tail, i)
  }

  def afterDelay(delay: Int)(block: => Unit) = {
    val item = new WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() {
    (agenda: @unchecked) match {
      case item :: rest =>
        curTime = item.time
        agenda = rest
        item.action()
    }
  }

  def run() {
    afterDelay(0) {
      println("Simulation started. Time is " + currentTime)
    }
    while (!agenda.isEmpty) next()
  }
}
