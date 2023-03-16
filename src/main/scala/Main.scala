object Main {

  //  Theory

  def mapFun[T, U](lst: List[T], f: T => U): List[U] = (lst foldRight List[U]())((x, y) => f(x) :: y)

  def mapFunFL[T, U](lst: List[T], f: T => U): List[U] = lst.foldLeft(List[U]())((acc, x) => acc :+ f(x))

  def lengthFun[T](lst: List[T]): Int = (lst foldRight 0)((_, acc) => acc + 1)

  def lengthFunFL[T](lst: List[T]): Int = (lst foldLeft 0)((acc, _) => acc + 1)

  //  Exercise 1
  //
  //  1.1.The concatenation operator(++) can be applied to generic Lists and both logical conjunction
  //  (&&) and logical disjunction (||) operators could be applied to a boolean List.
  //     Write methods to represent these behaviors using

  //     i) foldRight

  def concat[A](lst1: List[A], lst2: List[A]): List[A] = (lst1 foldRight lst2)((h, t) => h :: t) // or (_ :: _)

  def concat2[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
    case Nil => lst2
    case h :: t => h :: concat2(t, lst2)
  }

  def conjFR[A](l: List[Boolean]): Boolean = (l foldRight true)(_ && _) // ((x, acc) => x && acc)

  def disjFR[A](l: List[Boolean]): Boolean = (l foldRight false)(_ || _)

  //     ii) foldLeft

  def concatFL[A](lst1: List[A], lst2: List[A]): List[A] = (lst2 foldLeft lst1)((h, t) => h :+ t)

  def conjFL[A](l: List[Boolean]): Boolean = (l foldLeft true)(_ && _) // ((x, acc) => x && acc)

  def disjFL[A](l: List[Boolean]): Boolean = (l foldLeft false)(_ || _)

  //  1.2.Define an additional version of the remDup polymorphic / generic method that eliminates
  //  consecutive duplicates of a list of elements using foldRight and dropWhile.

  def remDup2[A](l: List[A]): List[A] = {
    (l foldRight List[A]())((x, acc) => x :: acc.dropWhile(_ == x)) // (y => y == x)
  }

  //  Exercise 2

  //  It is intended to keep information about the results of the matches of a soccer championship day
  //  in the following data structure:

  type Team = String
  type Goals = Int
  type Match = ((Team, Goals), (Team, Goals))
  type Fixtures = List[Match]

  //  Define the following methods using foldLeft or foldRight:
  //
  //    a) noItself which checks that no team plays with itself.

  def noItself(f: Fixtures): Boolean = (f foldRight true)((x, acc) => x._1._1 != x._2._1 && acc)

  //    b) withoutRep which checks that no team plays more than one game.
  //    c) teams which gives the list of teams participating in the Fixtures.
  //    d) draws which gives lists of pairs of teams that tied for the day.
  //    e) points which calculates the points that each team obtained in the Fixtures
  //      (won - 3 points; lost – 0 points; tied - 1 point).
  //  The function should return a value of type: List[(Team, Int)]


  // main for testing purposes

  def main(args: Array[String]): Unit = {

    val f = List((("eq2", 3), ("eq3", 5)), (("eq5", 3), ("eq6", 5)))
    println(noItself(f))
  }
}