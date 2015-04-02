package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // My solution
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => l // note: authors' solution throws exception
      case Cons(_, t) => t
    }

  // My solution for setHead: I read it wrong, I thought it meant to insert and element
  // at 0, but the book says to replace the element at 0.
  def MySetHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(prev_h, t) => Cons(h, Cons(prev_h, t))
    }

  // Authors' solution
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("...")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def my_dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) my_dropWhile(t, f)
        else l
    }

  // Authors' solution
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // Exercise 3.6
  // Not everything works out so nicely. Implement a function, init, that
  // returns a List consisting of all but the last element of a List. So,
  // given List(1,2,3,4), init will return List(1,2,3). Why can’t this
  // function be implemented in constant time like tail?
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("...")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  // Ex. 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Ex. 3.11: sum, product, and length using foldLeft
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def prodLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  // Ex 3.12: reverse using a fold method
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((as, a) => Cons(a, as))

  // Ex 3.14: append using foldRight
  // This works, but it's not nearly as elegant as the authors' solution.
  def appendRight[A](a1: List[A], a2: List[A]): List[A] = {
    val ax = foldRight(a2, List[A]())((a, as) => Cons(a, as))
    foldRight(a1, ax)((a, as) => Cons(a, as))
  }

  // Ex 3.15 concatenate a list of lists into a single list.
  def flatten[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())((as, acc) => {
      append(as, acc)
    })
  }

  // Ex 3.16 add 1 to each element
  def add1(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, add1(xs))
    }
  // Authors' way of doing this uses foldRight
  def add1_authors(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  // Ex 3.17 turns each value in a List[Double] into a String
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((d,t) => Cons(d.toString, t))

  // Ex 3.18 map
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  // Ex 3.19 filter
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,acc) => if (f(h)) Cons(h,acc) else acc)

  // Ex 3.20 flatMap
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil:List[A])((h,acc) => {
      // h is a list
    }
  }
}
