package LazyEvaluation

object Primes extends App{
  def from(n: Int): Stream[Int] = n #:: from(n + 1)
  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
  }
  val primes = sieve(from(2))
  
  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }
  
  val nats = from(0)
  val m4s = nats map (_ * 4)
}