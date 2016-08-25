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
    val numcups = capacities.length
    val actions =
      (for (cup <- (0 until numcups)) yield Empty(cup)) ++
        (for (cup <- (0 until numcups)) yield Fill(cup)) ++
        (for (from <- (0 until numcups); to <- (0 until numcups) if (to != from)) yield new Pour(from, to))

    //----------------------------------------------------------------------
    // Possible states after repeating transitions.
    //----------------------------------------------------------------------
    def transitions(states: Set[State]): Stream[Set[State]] = {
      if (states.isEmpty) Stream.empty
      else {
        val child = for {
          state <- states
          action <- actions
        } yield action.next(state)
        println(child)
        (child #:: transitions(child))
      }
    }
    //----------------------------------------------------------------------
    // Test if target amount is obtained.
    // ... Need to know what steps will yield the result.
    //----------------------------------------------------------------------
    def test(target: Int): Stream[State] = for {
        states <- transitions(Set(initialState))
        state <- states
        amount <- state if (amount == target)

      } yield {
        state
      }
  }
  val p = new Pouring(Vector(3, 5, 9))
  println(p.actions)
  println(p.test(7).take(2).toList)
}