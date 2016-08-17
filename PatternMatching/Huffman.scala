package PatternMatching

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {
  val BIT_L: Int = 0
  val BIT_R: Int = 1
  type Counter = List[(Char, Int)] // Character counter ('A', 8) or ('B', 3)

  /**
   * ********************************************************************************
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   * ********************************************************************************
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree {
    override def toString = "{" + left.toString + "Fork" + right.toString + "}"
  }
  case class Leaf(char: Char, weight: Int) extends CodeTree {
    override def toString = "{%c,%d}".format(char, weight)
  }

  // Part 1: Basics
  //--------------------------------------------------------------------------------
  // Total weight of a given Huffman tree
  //--------------------------------------------------------------------------------
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(c, w) => w
    case Fork(l, r, s, w) => w
  }

  //--------------------------------------------------------------------------------
  // List of characters define in a given Huffman tree
  //--------------------------------------------------------------------------------
  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, w) => List(c)
    case Fork(l, r, s, w) => s
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * ********************************************************************************
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   * ********************************************************************************
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    //--------------------------------------------------------------------------------
    // Build a list of Counter ('A', 8) -> ('B', 3) -> ('E', 1) -> ...
    // Given a character (such as 'B'), look for the counter for 'B' ('B', num).
    // If exists, replace it with ('B', num+1) resulting in a new list.
    //--------------------------------------------------------------------------------
    def buildCounterList(c: Char, list: List[(Char, Int)]): List[(Char, Int)] = list match {
      case Nil => {
        List((c, 1))
      }
      case h :: t => {
        //--------------------------------------------------------------------------------
        // If the character matches, then increase the count. 
        // Otherwise, recurse into the rest of the counter list until found a match or Nil.
        // If found Nil, then there is no counter for the character yet, hence add it, 
        // which is handled by Nil case above.
        //--------------------------------------------------------------------------------
        if (h._1 == c) (c, h._2 + 1) :: list.tail
        else h :: buildCounterList(c, list.tail)
      }
    }
    //--------------------------------------------------------------------------------
    // For each character in the text (List[Char]), build up a counter (ch, count): Counter.
    //--------------------------------------------------------------------------------
    // foldLeft[B](initial: B)(f: (B, A) ⇒ B): B
    // foldLeft(Counter)(f: (Counter, Char) => Counter): Counter
    //--------------------------------------------------------------------------------
    if (chars.isEmpty) {
      Nil
    } else {
      chars.foldLeft(Nil: Counter)((_counter, _ch) => buildCounterList(_ch, _counter))
    }
  }

  /**
   * ********************************************************************************
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   * ********************************************************************************
   */
  /*
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    //--------------------------------------------------------------------------------
    // Build a sorted list of Leaf.
    //--------------------------------------------------------------------------------
    def sort(in: List[(Char, Int)]): List[Leaf] = in match {
      case List() => List()
      case h :: t => insert(h, sort(t))
    }
    //--------------------------------------------------------------------------------
    // Insert Leaf in the sorted list of Leaf.
    //--------------------------------------------------------------------------------
    def insert(c: (Char, Int), list: List[Leaf]): List[Leaf] = list match {
      case List() => List(new Leaf(c._1, c._2))
      case h :: t => {
        //--------------------------------------------------------------------------------
        // If C:[char, counter] is smaller, prepend it to the List[Leaf].
        // Otherwise, recurse call insert to find a position in the tail of the list.
        //--------------------------------------------------------------------------------
        if (c._2 <= h.weight) new Leaf(c._1, c._2):: list
        else h :: insert(c, t)
      }
    }
    sort(freqs)
  }
	*/
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case List() => List()
    case h :: t => {
      //--------------------------------------------------------------------------------
      // Insert Leaf in the sorted list of Leaves.
      //--------------------------------------------------------------------------------
      def insert(c: (Char, Int), list: List[Leaf]): List[Leaf] = list match {
        case List() => List(new Leaf(c._1, c._2))
        case h :: t => {
          //--------------------------------------------------------------------------------
          // If C:[char, counter] is smaller, prepend it to the List[Leaf].
          // Otherwise, recurse call insert to find a position in the tail of the list.
          //--------------------------------------------------------------------------------
          if (c._2 <= h.weight) new Leaf(c._1, c._2) :: list
          else h :: insert(c, t)
        }
      }
      insert(h, makeOrderedLeafList(t))
    }
    case _ => throw new IllegalStateException
  }
  /**
   * ********************************************************************************
   * Checks whether the list `trees` contains only one single code tree.
   * ********************************************************************************
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    //println("Singleton tree = %s".format(trees))
    (trees.length == 1)
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    //--------------------------------------------------------------------------------
    // Find a position for c in CodeTree list where order of the weights is preserved. 
    //--------------------------------------------------------------------------------
    def insert(c: CodeTree, list: List[CodeTree]): List[CodeTree] = list match {
      case Nil => List(c)
      case h :: t => {
        if (weight(c) < weight(h)) c :: list
        else h :: insert(c, t)
      }
    }
    //--------------------------------------------------------------------------------
    // Create a Fork from the first two, then create a list where weights is ordered.
    //--------------------------------------------------------------------------------
    if (trees.length < 2) trees
    else insert(makeCodeTree(trees.head, trees.tail.head), trees.tail.tail)
  }

  /**
   * ********************************************************************************
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   * ********************************************************************************
   */
  def until(singleton: (List[CodeTree] => Boolean), combine: (List[CodeTree] => List[CodeTree]))(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees)) trees
    else until(singleton, combine)(combine(trees))
  }

  /**
   * ********************************************************************************
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   *
   * [NOTE]
   * If there is only one character in the text, only one Leaf is returned, and
   * it only serves to tell the number of occurrences.
   * *******************************************************************************
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val list: List[CodeTree] = until(singleton, combine)(makeOrderedLeafList(Huffman.times(chars)))
    list.head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * ********************************************************************************
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   * *******************************************************************************
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    //--------------------------------------------------------------------------------
    // While decoding 11-010-1101 (ABC), a sub tree of the Code Tree is being used.
    // When a code 11 (A) is decoded, then need to restart the decoding form the top 
    // of the Code Tree for the next ode 010.
    // top is the entire code tree, and tree is the sub tree.
    //--------------------------------------------------------------------------------
    val top: CodeTree = tree
    def decorder(tree: CodeTree, bits: List[Bit]): List[Char] = tree match {
      case Leaf(c, w) => {
        if (bits.isEmpty) List(c)
        else c :: decorder(top, bits)
      }
      case Fork(left, right, s, w) => {
        if (bits.isEmpty) Nil
        else if (bits.head == BIT_L) decorder(left, bits.tail)
        else decorder(right, bits.tail)
      }
    }
    decorder(tree, bits)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * ********************************************************************************
   * This function encodes `text` using the code tree `tree` into a sequence of bits.
   * Along the way, when a left branch is chosen, a 0 is added to the representation,
   * and when a right branch is chosen, 1 is added to the representation.
   *
   * [Clarification]
   * What if the text contains only one character?
   * *******************************************************************************
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    //--------------------------------------------------------------------------------
    // Encode each character in the text (List[Char])
    //--------------------------------------------------------------------------------
    // [How list.foldLeft(IB)(f) works]
    // Keep applying f(B(n-1), list.head) => B(n) where B(n-1) is previous result of f.
    // For the first application of f, there is no previous result of f. Hence use IB.
    //
    // list.foldLeft[B](initial_value: B)(f: (B, A) => B): B
    //--------------------------------------------------------------------------------
    // type CODE List[Bit]
    // foldLeft(CODE)(f: (CODE, Char) => CODE): CODE
    // 
    // Keep applying f(CODE(n-1), c:Char) which encode c into binary sequence.
    // Use Nil as the initial CODE, providing Nil:List(Bit) => List(Bit).
    //--------------------------------------------------------------------------------
    type CODE = List[Bit]
    text.foldLeft(Nil: CODE)((code: CODE, target: Char) => code ::: encoder(tree)(target))
  }
  //--------------------------------------------------------------------------------
  // Create an instance of encoder for a code tree given.
  // A encoder take a character and return binary sequence code.
  //--------------------------------------------------------------------------------
  def encoder(tree: CodeTree)(target: Char): List[Bit] = tree match {
    //--------------------------------------------------------------------------------
    // Only Fork node can identify 0/left or 1/right. Hence apply f(Leaf) only tells
    // character is found (otherwise it is illegal state) and returns Nil to mark end.
    // [Note]
    // Binary code can only exists if there are more than 1 characters in the text.
    // Otherwise, there is only one Leaf in the code tree which only servers to tell
    // the weight of the character.
    //--------------------------------------------------------------------------------
    case Leaf(c, w) => {
      require(c == target, "%c does not exist in the code tree".format(target))
      Nil
    }
    case Fork(left, right, str, w) => {
      //println("In encorder target = %c, left = %s, right = %s".format(target, left.toString, right.toString))
      if (chars(left).contains(target)) BIT_L :: encoder(left)(target)
      else if (chars(right).contains(target)) BIT_R :: encoder(right)(target)
      else throw new IllegalStateException("%c does not exist in the code tree".format(target))
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    /*
    val c = (table.filter(_._1 == char))
    if(!c.isEmpty) c(0)._2 else Nil
    */
    if (table.isEmpty) Nil
    else if (table.head._1 == char) table.head._2
    else codeBits(table.tail)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    // Left is 0, Right is 1
    case Leaf(c, w) => List[(Char, List[Bit])]()
    case Fork(left, right, str, w) => {
      //--------------------------------------------------------------------------------
      // Create a code table. If current code is 00, then left is 00+0, right is 00+1.
      //--------------------------------------------------------------------------------
      def create(tree: CodeTree, code: List[Bit]): CodeTable = tree match {
        case Leaf(c, w) => List((c, code))
        case Fork(left, right, str, w) => mergeCodeTables(create(left, code ::: List(BIT_L)), create(right, code ::: List(BIT_R)))
      }
      mergeCodeTables(create(left, List(BIT_L)), create(right, List(BIT_R)))
    }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text.foldLeft(List[Bit]())((initial, char) => initial ::: codeBits(convert(tree))(char))
  }
}

//--------------------------------------------------------------------------------
// Personal test code
//--------------------------------------------------------------------------------
import PatternMatching.Huffman._
object TestHuffman extends App {

}
