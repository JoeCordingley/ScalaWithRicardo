package euler

object Euler3 {
  //The prime factors of 13195 are 5, 7, 13 and 29.
  //What is the largest prime factor of the number 600851475143 ?


  def largestPrimeFactor(n: Long): Long = LazyList.from(2).takeWhile(i => i*i <= n).find(d => n%d == 0) match {
    case Some(d) => largestPrimeFactor(n / d)
    case None => n
  }

//  def largestPrimeFactor2(n: Long): Long = {
//    getPrimeFactors(n).max
//  }
//
//  def getPrimeFactors(l: Long): List[Long] = primes.takeWhile(p => p*p <= l).filter(p => l %p == 0)

 // val primes: List[Long] = ???

  List(1,2,3).collect{
    case x if x%2 ==0 => x*x
  }
  List(1,2,3).exists{
    x => x%2 ==0
  }

  val xs = List(1,2,3)

  xs.foldLeft[Option[Int]](None){
    case (None, a) => Some(a)
    case (Some(x), y) => Some(x max y)
  }

  def foldLeft[A, B](l: List[A])(z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs)(f(z,x))(f)
  }

//  List(1,2,3)(None)
//  List(2,3) Some(1)
//  List(3) Some(2)
//  Nil Some(3)
//  Some(3)

  def primeFactors(n: Long): List[Long] = LazyList.from(2).takeWhile(i => i*i <= n).find(d => n%d == 0) match {
    case None => List(n)
    case Some(d) => d :: primeFactors(n / d)
  }

  List(1, 2, 3) match {
    case Nil => "empty"
    case x :: xs => "x is 1 xs is List(2,3)"
    case List(x,y,z) => "x is 1 y is 2 z is 3"
    case x :: y :: z :: Nil => "same as above"
  }

  LazyList(0) #::: LazyList(1,2,3)


  def main(args: Array[String]): Unit = {
    val input: Long = 600851475143L

    val answer = largestPrimeFactor(input)

    println(answer)
  }
}
