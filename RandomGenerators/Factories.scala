package RandomGenerators

//--------------------------------------------------------------------------------
// Function Programing Design in Scala.
// https://www.coursera.org/learn/progfun2/lecture/S7NQA/lecture-1-3-functional-random-generators
//--------------------------------------------------------------------------------
object Factories extends App {
  println("hello")
  trait Factory[+T] {
    self => // alias of 'this'
    //val hoge = this
    def generate: T
    //--------------------------------------------------------------------------------
    // Synthesize a new generate of type S based on the Factory of this instance.
    // if the Factory of this class is f: T, such as f: Int = java.util.Random.nextInt(),
    // then if we can create a function g: T => S, then g(f) is used as the Factory for S based on T.
    //--------------------------------------------------------------------------------
    def map[S](f: T => S): Factory[S] = new Factory[S] {
      //def generate = f(Factory.hoge.generate) // Error with value Factory is not found
      //def generate = f(hoge.generate) // Error with Type mismatch S required: T. 
      //def generate = f(generate) // Infinite recurse by generate is expanded to this.generate referring to 'def'ined generate itself. 
      def generate = f(self.generate)
    }
    def flatMap[S](f: T => Factory[S]): Factory[S] = new Factory[S] {
      def generate = f(self.generate).generate
    }
  }
  //-------------------------------------------------------------------------------- 
  // Utilities
  //-------------------------------------------------------------------------------- 
  def single[T](x: T): Factory[T] = new Factory[T] { def generate = x }
  def choose(lo: Int, hi: Int): Factory[Int] = for (x <- intFactory) yield lo + x % (hi - lo)
  def oneOf[T](xs: T*): Factory[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

  //-------------------------------------------------------------------------------- 
  // Int factory
  //-------------------------------------------------------------------------------- 
  val intFactory = new Factory[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }
  for (i <- intFactory) yield i
  //-------------------------------------------------------------------------------- 
  // Boolean Factory based on Int Factory (java.util.Random.nextInt())
  // by applying transformation from Int to Boolean.
  //-------------------------------------------------------------------------------- 
  def int2Bool(i: Int): Boolean = (i > 0)
  val boolFactory = intFactory.map(int2Bool)
  //println(boolFactory.generate)

  //-------------------------------------------------------------------------------- 
  // List factory
  //-------------------------------------------------------------------------------- 
  def lists: Factory[List[Int]] = for {
    isEmpty <- boolFactory
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
  def emptyLists = single(Nil)
  def nonEmptyLists = for {
    head <- intFactory
    tail <- lists
  } yield head :: tail
  //println(lists.generate)

  //-------------------------------------------------------------------------------- 
  // Tree factory
  //-------------------------------------------------------------------------------- 
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree
  def intToLeaf(i: Int): Leaf = {
    new Leaf(i)
  }
  def leafFactory: Factory[Leaf] = intFactory.map(intToLeaf)
  def innerFactory: Factory[Inner] = new Factory[Inner] {
    def generate = new Inner(treeFactory.generate, treeFactory.generate)
  }
  def treeFactory: Factory[Tree] = for {
    isLeaf <- boolFactory
    tree <- if (isLeaf) leafFactory else innerFactory
  } yield tree
  
  def treeFactory2: Factory[Tree] = {
    boolFactory.flatMap(isLeaf => if(isLeaf) leafFactory else innerFactory)
  }
}