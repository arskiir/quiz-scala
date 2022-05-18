object Q1 extends App {
  def partition(
      l: List[Int],
      f: Int => Boolean,
      acc: List[List[Int]] = List(List(), List())
  ): List[List[Int]] = {
    if (l.isEmpty) acc
    else if (f(l.head))
      partition(l.tail, f, List(acc.head ++ List(l.head), acc.tail.head))
    else
      partition(
        l.tail,
        f,
        List(acc.head, acc.tail.head ++ List(l.head))
      )
  }

  def f1(x: Int): Boolean = { x % 2 == 1 }
  def f2(x: Int): Boolean = { x * x > 10 }

  val l1 = List(1, 2, 3, 4, 5)
  println(partition(l1, f1)) // List(List(1, 3, 5), List(2, 4))
  println(partition(l1, f2)) // List(List(4, 5), List(1, 2, 3))
  println(partition(l1, (x => x == 0))) // List(List(), List(1, 2, 3, 4, 5))
  println(partition(l1, (x => x < 6))) // List(List(1, 2, 3, 4, 5), List())
}
