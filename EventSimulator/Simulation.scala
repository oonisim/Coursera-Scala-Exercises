/**
 * Discrete Event Simulator
 * https://www.coursera.org/learn/progfun2/lecture/WFKfD/lecture-3-5-discrete-event-simulation-api-and-usage
 */

package EventSimulator


trait Simulation {
  //--------------------------------------------------------------------------------
  // An Action is a function that takes no parameter and returns Unit.
  // It just change the internal state.
  //--------------------------------------------------------------------------------  
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0
  def currentTime: Int = curtime
  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }
  private def loop(): Unit = agenda match {
    case first :: rest => {
      agenda = rest
      curtime = first.time
      //println("firing action %s signature %s".format(first.action, first.action.toString))
      first.action()
      loop()
    }
    case Nil =>
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }
  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if (first.time <= item.time) =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }
}



