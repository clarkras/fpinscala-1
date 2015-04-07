package fpinscala.datastructures

object RunTree {
  import Tree._

  val leftTree = Branch(Leaf(3), Leaf(8))
  val rightTree = Branch(Leaf(2), Leaf(6))
  val mainTree = Branch(leftTree, rightTree)
  def main(args: Array[String]): Unit = {
    println("size        " + size(leftTree))
    println("size        " + size(rightTree))
    println("size        " + size(mainTree))
    println("max         " + maximum(mainTree))
    println("depth       " + depth(leftTree))
    println("depth       " + depth(mainTree))
    println("map         " + map(mainTree)(_ * 10))
    println("sizeViaFold " + sizeViaFold(mainTree))
    println("maxViaFold  " + maxViaFold(mainTree))
    println("depthViaFold " + depthViaFold(mainTree))
  }
}
