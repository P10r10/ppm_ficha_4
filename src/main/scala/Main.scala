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


  // main for testing purposes

  def main(args: Array[String]): Unit = {

    val lst1 = List(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 4, 5, 5, 5, 6)

    println(remDup2(lst1))
  }
}