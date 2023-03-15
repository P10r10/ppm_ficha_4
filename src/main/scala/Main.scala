object Main {

  //  Exercise 1
  //
  //  1.1.The concatenation operator(++) can be applied to generic Lists and both logical conjunction
  //  (&&) and logical disjunction (||) operators could be applied to a boolean List.
  //    Write methods to represent these behaviors using
  //    i) foldRight
  //    ii) foldLeft
  def my_sum(lst: List[Int]):Int = (lst foldRight 1) (_ * _)

  def main(args: Array[String]): Unit = {
    val lst = List(1, 2, 3, 4, 5)

    println(my_sum(lst))
  }
}