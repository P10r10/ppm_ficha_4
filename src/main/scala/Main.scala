object Main {

  //  Exercise 1
  //
  //  1.1.The concatenation operator(++) can be applied to generic Lists and both logical conjunction
  //  (&&) and logical disjunction (||) operators could be applied to a boolean List.
  //    Write methods to represent these behaviors using
  //    i) foldRight

  def concat[A](lst1: List[A], lst2: List[A]):List[A] = (lst1 foldRight lst2) ((h, t) => h :: t)

  def concat2[A](lst1: List[A], lst2: List[A]):List[A] = lst1 match {
    case Nil => lst2
    case h :: t => h :: concat2(t, lst2)
  }

  //    ii) foldLeft

  def concatFL[A](lst1: List[A], lst2: List[A]):List[A] = (lst2 foldLeft  lst1) ((h, t) => h :+ t)

  def main(args: Array[String]): Unit = {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(7, 8, 9, 10, 11)
    val lst3 = List(true, true)
    val lst4 = List(false, false)

    println(concatFL(lst1, lst2))
  }
}