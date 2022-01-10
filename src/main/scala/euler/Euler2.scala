package euler

object Euler2 {


  val primes: LazyList[Int] = 2 #:: LazyList.from(3)
    .filter( i => primes.takeWhile(p => p*p <= i).forall(p => p%i != 0))

  def main(args: Array[String]): Unit = {

    lazy val fibonacci : LazyList[Int] = 0 #:: 1 #:: fibonacci.zip(fibonacci.tail) // (Fib1, Fib2), (Fib2, Fib3), (Fib3, Fib4) ...
      .map{
        case (x,y) => x+y
      }
    //Fib1 = 0, Fib2 = 1, Fib(N) = Fib(N - 1) + Fib(N -2)


    val answer = fibonacci
      .takeWhile(v => v <= 4000000)
      .filter(f => f % 2 == 0)
      .sum

    println(answer)
  }


}
