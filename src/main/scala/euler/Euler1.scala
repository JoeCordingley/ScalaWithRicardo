package euler

object Euler1 {
  //sum of multiples of 3 or 5 below 1000

  def main(args: Array[String]): Unit = {
    val oneToAThousand: List[Int] = (1 until 1000).toList

    val divisors: List[Int] = List(3,5,7)

    case class A(i: Int)
    val x = (1 to 100).toList.map(i => A(i))

    val answer: Int = oneToAThousand.filter(
      v => divisors.exists{d => v % d == 0}
    ).sum

    println(answer)
  }

}
