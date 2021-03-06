package euler

import cats._
import cats.data._
import cats.implicits._

object Cats {

  //Monad
  //Applicative (Functor)
  //Functor

  //Monoid
  //Semigroup

  //List
  //Option
  //Future -> IO

  //cats-mtl


//  def flatMapOpt[A, B](o: Option[A])(f: A => Option[B]): Option[B] = o match {
//    case None => None
//    case Some(a) => f(a)
//  }
//
//  def flatMapList[A, B](o: List[A])(f: A => List[B]): List[B] = o match {
//    case Nil => Nil
//    case x :: xs => f(x).concat(flatMapList(xs)(f))
//  }
//
//  trait FlatMap[F[_]]{
//    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
//  }
//
//  val OptFlatMap: FlatMap[Option] = new FlatMap[Option]{
//    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = flatMapOpt(fa)(f)
//  }
//
//  //Eq is it equal?
//  //Order gt eq or lt
//  //Show print
//  trait Semigroup[A] {
//    def combine(x: A, y: A): A
//    //combine(x, combine(y,z)) === combine(combine(x,y), combine(z)) associativity
//    //combine(x,y) != combine(y,x) commutativity
//  }
//
//  implicit val intSemigroup: Semigroup[Sum] = {
//    case (Sum(x), Sum(y)) => Sum(x + y)
//  }
//
//  trait Monoid[A] extends Semigroup[A]{
//    def empty: A
//  }
//
//  case class Sum(i: Int)

  def divide(x: Double, y: Double): Option[Double] = if (y == 0) None else Some(x/y)

  val l: Option[List[Double]] =  List(1d,2d,3d).traverse(divide(10, _))



  //cats.data
  //Reader
  //State
  //Validated
  //

//  case class State[S, A](run: S => (S, A))
//
//  def get[S]: State[S, S] = State(s => (s, s))
//  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
//
//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get[S]
//    _ <- set(f(s))
//  } yield ()
//
//  implicit def stateMonad[S]: Monad[State[S, *]] = new Monad[State[S, *]]{
//    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = State{ s =>
//      val (s1, a) = fa.run(s)
//      val (s2, b) = f(a).run(s1)
//      (s2, b)
//    }
//
//    override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] = ???
//
//    override def pure[A](x: A): State[S, A] = State(s => (s, x))
//  }

  //State S => (S, A)


  def common2(x: String, y: String): String = {

    type Chars = List[Char]
    type Input = (Chars, Chars)
    type Cache = Map[Input, Chars]
    type MyState[A] = State[Cache, A]

    def addAnswer(input: Input, ans: Chars): MyState[Unit] = State.modify[Cache](_ + (input -> ans))
    def getAnswer(input: Input): MyState[Option[Chars]] = State.inspect(_.get(input))

    def inner(input: Input): MyState[Chars] = for {
      maybeAns <- getAnswer(input)
      ans <- maybeAns match {
        case Some(ans) => State.pure[Cache, List[Char]](ans)
        case None => for {
          ans <- calculate(input)
          _ <- addAnswer(input, ans)
        } yield ans
      }
    } yield ans

    def calculate: Input => MyState[Chars] = {
      case (x :: xs, y :: ys) if x == y => inner(xs, ys).map(x :: _)
      case (x :: xs, y :: ys) => for {
        remLeft <- inner(xs, y :: ys)
        remRight <- inner(x :: xs, ys)
      } yield if (remLeft.length > remRight.length) remLeft else remRight
      case (Nil, _)  | (_, Nil) => State.pure(Nil)
    }

    type CalcFun = Input => Chars

    inner(x.toList,y.toList).runA(Map.empty).value.mkString
  }


  def common(x: String, y: String): String = {
    def inner(l1 :List[Char], l2 : List[Char]): List[Char] = (l1, l2) match{
      case (Nil, _) | (_, Nil) => Nil
      case (x::xs, y::ys) => if (x == y) x::inner(xs, ys) else {
       val remLeft = inner(xs, y::ys)
       val remRight = inner(x::xs, ys)
        if(remLeft.length > remRight.length) remLeft else remRight
      }
    }
    inner(x.toList,y.toList).mkString
  }

  implicit val additionMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  case class Max[A](a: A)

  implicit def maxSemigroup[A: Order]: Semigroup[Max[A]] = new Semigroup[Max[A]]{
    override def combine(x: Max[A], y: Max[A]): Max[A] = Max(Order[A].max(x.a, y.a))
  }

  //had to remove because its already in cats implicits
//  implicit def optMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]]{
//    override def empty: Option[A] = None
//
//    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
//      case (None, y) => y
//      case (x, None) => x
//      case (Some(x), Some(y)) => Some(x |+| y)
//    }
//  }

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A,B)] = new Monoid[(A, B)]{
    override def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (x, y) match {
      case ((lA, lB), (rA, rB)) => (lA |+| rA, lB |+| rB)
    }
  }


  //List(1,2,3).max fine but unsafe generally
  //List[Int]().max throws error




  List[Int]().foldMap[Option[Max[Int]]](i => Some(Max(i))).map(_.a)

  val x: Monoid[Option[Max[Int]]] = implicitly
  val y: Semigroup[Max[Int]] = implicitly

  case class Mult(value: Int)

  implicit val multMonoid: Monoid[Mult] = new Monoid[Mult] {
    override def combine(x: Mult, y: Mult): Mult = Mult(x.value * y.value)

    override def empty: Mult = Mult(1)
  }

  List(1,2,3).foldMap(Mult).value

  def main(args: Array[String]): Unit = {
    println(common2("abcaccadacdacadcadcadcacdacad", "dcdcaedcacddacbaaccadacadcacadcacadaca"))
    val i: Monoid[Int] = Monoid[Int]

    println(i.combine(1, 2))
  }


}
