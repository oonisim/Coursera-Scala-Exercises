//----------------------------------------------------------------------
// Coursera Functional Program Design in Scala 
// https://www.coursera.org/learn/progfun2/lecture/EkEqR/lecture-2-5-case-study-the-water-pouring-problem 
//----------------------------------------------------------------------

package LazyEvaluation

object PouringTest extends App {
  class Pouring(capacities: Vector[Int]) {

    // States
    type State = Vector[Int]
    val initialState = capacities.map(e => 0)
    //----------------------------------------------------------------------
    // Moves
    //----------------------------------------------------------------------
    trait Move {
      def next(previous: State): State
    }
    case class Empty(cup: Int) extends Move {
      def next(previous: State) = previous.updated(cup, 0)
    }
    case class Fill(cup: Int) extends Move {
      def next(previous: State) = previous.updated(cup, capacities(cup))
    }
    case class Pour(from: Int, to: Int) extends Move {
      def next(previous: State) = {
        // if to can take entire amount of from (capacit-to > from) then from.
        val amount = (capacities(to) - previous(to)).min(previous(from))
        previous
          .updated(to, previous(to) + amount)
          .updated(from, previous(from) - amount)
      }
    }
    //----------------------------------------------------------------------
    // Available actions
    //----------------------------------------------------------------------    
    val numcups = (0 until capacities.length)
    val actions =
      (for (cup <- numcups) yield Empty(cup)) ++
        (for (cup <- numcups) yield Fill(cup)) ++
        (for (from <- numcups; to <- numcups if (to != from)) yield new Pour(from, to))

    //----------------------------------------------------------------------
    // Possible states after repeating transitions.
    //----------------------------------------------------------------------
    def transitions(states: Set[State], identified: Set[State]): Stream[Set[State]] = {
      if (states.isEmpty) Stream.empty
      else {
        val child = for {
          state <- states
          action <- actions
          /*
           * With guard,    elapsed time: 0.077111888s for taking pow 2^10 result. 
           * Without guard, elapsed time: 0.326833628s for taking pow 2^10 result. 
           */
          if (!identified.contains(action.next(state)))
        } yield action.next(state)
        //println(child)
        (child #:: transitions(child, identified ++ child))
      }
    }
    //----------------------------------------------------------------------
    // Test if target amount is obtained.
    // ... Need to know what steps will yield the result.
    //----------------------------------------------------------------------
    def test(target: Int): Stream[State] = for {
      states <- transitions(Set(initialState), Set(initialState))
      state <- states
      amount <- state if (amount == target)

    } yield {
      state
    }
  }
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / Math.pow(10, 9) + "s")
    result
  }

  //println(p.actions)
  val i = 10
  print("For pow %d ".format(i))
  time {
    var p = new Pouring(Vector(3, 5, 9))
    p.test(7).take(Math.pow(2, i).toInt).toList
  }
}