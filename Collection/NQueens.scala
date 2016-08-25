package Collection

object NQuseen extends App {
  println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet

  def queens(n: Int): Set[List[Int]] = {
    //--------------------------------------------------------------------------------
    // Check if a queen can be placed at the column at the row given the queen placement
    // up to the (row -1).
    //--------------------------------------------------------------------------------
    def isSafe(column: Int, placement: List[Int]): Boolean = {
      //--------------------------------------------------------------------------------
      // [Logic]
      // Find if there are available columns for a queen to be placed at the current 'row'.
      // The columns where queens have been placed (placement(j) where j: (0 until row)) are taken.
      // The right diagonal columns for all placement(j), which is placement(j) + (row -j)), are taken.
      // The left  diagonal columns for all placement(j), which is placement(j) - (row -j)), are taken.
      // If there are available columns left which include the target 'column', then true.
      //--------------------------------------------------------------------------------
      val row = placement.length
      //val left = (0 until row).map(j => placement(j) + (row - j)).toSet
      //val left = (0 until row).map(j => placement(j) - (row - j)).toSet
      val right = for (j <- (0 until row) if (placement(j) + (row - j) < n)) yield placement(j) + (row - j)
      val left  = for(j <- (0 until row) if(placement(j) - (row - j) >= 0)) yield placement(j) - (row - j)
      val all = (0 until n).toSet
      val taken = (placement.toSet ++ left.toSet ++ right.toSet)
      if ((all -- taken).contains(column)) true
      else false
    }

    //--------------------------------------------------------------------------------
    // Find out possible queen positions at the row.
    // [Data Structure]
    // placement : List[Int] is one possible placement of queens.
    // - List index is row number (0 to n -1)
    // - placement(row) is the position of a queen is placed at the row. 
    //
    // Example: At row 0, a queen is placed at column 5. At row 2, column 0.
    // Rows      :  0, 1, 2, 3, 4, 5, 6, 7
    // Placement : (5, 2, 0, 7, 3, 1, 6, 4)
    //--------------------------------------------------------------------------------
    def placeQueensAt(row: Int): Set[List[Int]] = {
      if (row < 0) Set(List())
      else {
        val placements: Set[List[Int]] = for {
          placement <- placeQueensAt(row - 1)
          col <- 0 until n
          if (isSafe(col, placement))
        } yield {
          placement ::: List(col)
        }
        println("Possible positions up to row %d is %s".format(row, placements))
        placements
      }
    }
    placeQueensAt(n - 1)
  }

  //--------------------------------------------------------------------------------
  // Visualize the queen placement.
  //--------------------------------------------------------------------------------
  def visualize(placement: List[Int]) = {
    val lines = for (col <- placement) yield Vector.fill(placement.length)("*").updated(col, "X").mkString
    println("-----\n" + (lines mkString "\n"))
  }

  //--------------------------------------------------------------------------------
  // Run the N queen problem solution.
  //--------------------------------------------------------------------------------
  (queens(8)).map(visualize)
}