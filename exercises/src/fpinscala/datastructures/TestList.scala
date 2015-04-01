package fpinscala.datastructures

object TestList {
  import List._

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3)
    println("drop(1)     " + drop(l, 1))
    println("drop(2)     " + drop(l, 2))

    println("MySetHead   " + MySetHead(List(1,2,3), 0))

    println("Example 3.8 " + foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println("foldLeft    " + foldLeft(List(1,2,3), 0)(_ + _))
    println("foldLeft    " + foldLeft(List(1,2,3), 0)((z, x) => {
      println(s"   z: $z, x: $x")
      z + x
    }))
    println("reverse     " + reverse(List(1,2,3)))

  }
}
