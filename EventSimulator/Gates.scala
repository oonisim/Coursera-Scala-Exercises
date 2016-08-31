package EventSimulator

imp

import EventSimulator.Simulationort EventSimulator.copy.Simulation

abstract class Gates extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit = {
      def doit(f: Action): Unit = {
        //println("do it " + f.toString)
        f()
      }
      if (s != sigVal) {
        //sigVal = s; actions.foreach { x => x() }
        sigVal = s
        //actions.foreach(_())
        actions.foreach(doit)
      }
    }
    def addAction(a: Action): Unit = {
      //println("addAction a " + a + " is called")
      actions = a :: actions
      a()
    }
  }
  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }
    input.addAction(invertAction)
  }
  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        output.setSignal(in1Sig & in2Sig)
      }
    }
    in1.addAction(andAction)
    in2.addAction(andAction)
  }
  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(in1Sig | in2Sig)
      }
    }
    in1.addAction(orAction)
    in2.addAction(orAction)
  }
  def probe(name: String, wire: Wire): Unit = {
    /*
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    */
    val probeAction = new (() => Unit) {
      override def toString = "I am probe " + name + " " + wire
      def apply() = {
        println(s"$name $currentTime value = ${wire.getSignal}")
      }
    }
    wire.addAction(probeAction)
  }
}
