package fpinscala.datastructures

object TestList {
  import List._

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3)
    println("drop(1)        " + drop(l, 1))
    println("drop(2)        " + drop(l, 2))
    println("MySetHead      " + MySetHead(List(1,2,3), 0))
    println("Example 3.8    " + foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println("foldLeft       " + foldLeft(List(1,2,3), 0)(_ + _))
    println("foldLeft       " + foldLeft(List(1,2,3), 0)((z, x) => z + x))
    println("reverse        " + reverse(List(1,2,3)))
    println("reverse        " + reverse(List("a", "b", "c", 9)))
    println("appendRight    " + appendRight(List(1,2,3), List(4,5,6)))
    println("flatten        " + flatten(List(List(1,2,3), List(4,5,6))))
    println("add1           " + add1(List(1,2,3)))
    println("add1_authors   " + add1_authors(List(1,2,3)))
    println("doubleToString " + doubleToString(List(1.1,2.2,3.3)))
    println("map            " + map(List(1,2,3))(_ * 100))
    println("filter         " + filter(List(1,2,3,4,5))(_ % 2 == 0))

  }
}
