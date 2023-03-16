object Main {

  //  Theory

  def mapFun[T, U](lst: List[T], f:T => U): List[U] = (lst foldRight List[U]()) ((x, y) => f(x) :: y)

  def mapFunFL[T, U](lst: List[T], f:T => U): List[U] = lst.foldLeft(List[U]()) ((acc, x) => acc :+ f(x))

  def lengthFun[T](lst: List[T]): Int = (lst foldRight 0) ((_, acc) => acc + 1)

  def lengthFunFL[T](lst: List[T]): Int = (lst foldLeft  0) ((acc, _) => acc + 1)

  //  Exercise 1
  //
  //  1.1.The concatenation operator(++) can be applied to generic Lists and both logical conjunction
  //  (&&) and logical disjunction (||) operators could be applied to a boolean List.
  //     Write methods to represent these behaviors using
  //     i) foldRight

  def concat[A](lst1: List[A], lst2: List[A]):List[A] = (lst1 foldRight lst2) ((h, t) => h :: t) // or (_ :: _)

  def concat2[A](lst1: List[A], lst2: List[A]):List[A] = lst1 match {
    case Nil => lst2
    case h :: t => h :: concat2(t, lst2)
  }

  //     ii) foldLeft

  def concatFL[A](lst1: List[A], lst2: List[A]):List[A] = (lst2 foldLeft  lst1) ((h, t) => h :+ t)

  def main(args: Array[String]): Unit = {

    def x2(x: Int) = 2 * x

    val lst1 = List(1, 2, 3, 4)
    val lst2 = List(7, 8, 9, 10, 11)
    val lst3 = List(true, true)
    val lst4 = List(false, false)

    println(lst1.map(x2))
    println(mapFun(lst1, x2))
    println(mapFunFL(lst1, x2))
  }
}