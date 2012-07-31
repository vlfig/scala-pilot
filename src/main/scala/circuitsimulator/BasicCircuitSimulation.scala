package circuitsimulator

abstract class BasicCircuitSimulation extends Simulation {
  def InverterDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def signal = sigVal

    def setSignal(signal: Boolean) {
      if (signal != sigVal) {
        sigVal = signal
        actions.foreach {
          _()
        }
      }
    }

    def addAction(a: Action) = {
      println("new action")
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire) = {
    def invertAction() = {
      val inputSig = input.signal
      afterDelay(InverterDelay) {
        println("setting a signal")
        output setSignal !inputSig
      }
    }
    input addAction invertAction
  }

  def andGate(input1: Wire, input2: Wire, output: Wire) = {
    def andAction() = {
      val input = (input1.signal, input2.signal)
      afterDelay(AndDelay) {
        println("setting a signal")
        output.setSignal(input._1 & input._2)
      }
    }
    input1 addAction andAction
    input2 addAction andAction
  }

  def orGate(input1: Wire, input2: Wire, output: Wire) = {
    def orAction() = {
      val input = (input1.signal, input2.signal)
      afterDelay(OrDelay) {
        println("setting a signal")
        output.setSignal(input._1 | input._2)
      }
    }
    input1 addAction orAction
    input2 addAction orAction
  }

  def probe(name: String, wire: Wire) = {
    def probeAction() {
      println(name + " at " + currentTime + " is " + wire.signal)
    }

    wire addAction probeAction
  }

}

abstract class CircuitSimulation extends BasicCircuitSimulation {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object MySimulation extends CircuitSimulation {
  def InverterDelay = 1
  def AndDelay = 3
  def OrDelay = 5
  val input1, input2, sum, carry = new Wire

  def setup() {
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)
  }

  def main(args: Array[String]) {
    setup()
    run()
  }
}

