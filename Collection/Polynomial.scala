package Collection

object Polynomial {
  //--------------------------------------------------------------------------------
  // Polynomial function representation.
  // Y = f(X) = 5X^3 + 7X^2 -2X + 3 is representd as (0 -> 3, 1 -> -2, 2 -> 7, 3 -> 5)
  //--------------------------------------------------------------------------------
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    //--------------------------------------------------------------------------------
    // If a exponential (such as X^2) is missing, then coefficient for the exp is 0.
    // For example 5X^3 -2X + 3, it should become (0 -> 3, 1 -> -2, 2 -> 0, 3 -> 5)
    //--------------------------------------------------------------------------------
    val terms = terms0 withDefaultValue 0.0

    //--------------------------------------------------------------------------------
    // Polynomial number addtion
    //--------------------------------------------------------------------------------
    def +(other: Poly) = {
      def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
        // Get the coefficient for the exponential from other side.
        val (exp, coeff) = term
        //--------------------------------------------------------------------------------
        // Get the coefficient for the exponential from this side.
        // Add this.coefficient and other.coefficient for the expontial.
        //--------------------------------------------------------------------------------
        terms.updated(exp, (terms(exp) + coeff))
      }
      new Poly((other.terms foldLeft this.terms)(addTerm))
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }

  val a = new Poly(0 -> 3, 1 -> -2, 2 -> 7, 3 -> 5)
  //> a  : CollectionTest.MapTest.Poly = 5.0x^3 + 7.0x^2 + -2.0x^1 + 3.0x^0
  val b = new Poly(1 -> 1.0, 2 -> 2.0, 3 -> 5.0, 4 -> 6.0)
  //> b  : CollectionTest.MapTest.Poly = 6.0x^4 + 5.0x^3 + 2.0x^2 + 1.0x^1
  a + b //> res0: CollectionTest.MapTest.Poly = 6.0x^4 + 10.0x^3 + 9.0x^2 + -1.0x^1 + 3
  //| .0x^0
}