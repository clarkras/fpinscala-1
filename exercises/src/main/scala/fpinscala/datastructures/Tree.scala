package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + depth(l) max depth(r)
  }

  def map[A,B](t: Tree[A])(f: A => B ): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(left,right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def maxViaFold(t: Tree[Int]): Int = fold(t)(a => a)((l,r) => l max r)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 0)(1 + _ max _)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)))((l,r) => Branch(l,r))

}