package euler

object Euler3 {
  //The prime factors of 13195 are 5, 7, 13 and 29.
  //What is the largest prime factor of the number 600851475143 ?

  def primeMatch(n: Long): List[Long] = n match{
    case n % 2 == 0 => primeMatch(divideBy2(n))
    case
    }

  //i'm stuck, what i intended: making a function that would call itself, on every call looking for the next x where n % x == 0,
  //with x incrementing, and kept putting x onto a list, so that i can call maxNum on it
  def maxNum(List : List[Long]) : Long = List.max

  def divideBy2(num : Long) : Long = num / 2

  def main(args: Array[String]): Unit = {
    val input: Long = 600851475143L

    val answer = maxNum(primeMatch(input))

    println(answer)
  }
}
