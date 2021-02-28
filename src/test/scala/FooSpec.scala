import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.annotation.tailrec

class FooSpec extends AnyFlatSpec with should.Matchers {

  val listA = List(1, 2)
  val listB = List(3, 4)
  val listC = List(5, 6)
  val listD = List(7, 8, 9)

  def f(one: List[Int], two: List[Int]): List[List[Int]] = {
    // for (first <- one; second <- two) yield List(first, second)
    one.flatMap(first => two.map(second => List(first, second)))
  }

  def g(one: List[Int], two: List[Int], three: List[Int]): List[List[Int]] = {
    // for (first <- one; second <- two; third <- three) yield List(first, second, third)
    // for (perm <- f(one, two); third <- three) yield perm :+ third
    f(one, two).flatMap(perm => three.map(third => perm :+ third))
  }

  def combosTail(listOfLists: List[List[Int]]): List[List[Int]] = {
    def combine = (next: List[Int], acc: List[List[Int]]) => acc.flatMap(i => next.map(j => i :+ j))
    @tailrec
    def combosTailRec(next: List[Int], rest: List[List[Int]], acc: List[List[Int]]): List[List[Int]] = {
      if (rest.isEmpty) {
        combine(next, acc)
      } else {
        combosTailRec(rest.head, rest.tail, combine(next, acc))
      }
    }

    listOfLists match {
      case Nil => List(List())
      case one :: Nil => one.map(List(_))
      case one :: two :: tail => combosTailRec(two, tail, one.map(List(_)))
    }
  }

  "combosTailRec" should "permutate" in  {
    combosTail(List(listA, listB, listC)) should equal(
      List(
        List(1, 3, 5), List(1, 3, 6),
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      ))
  }

  def combos(listOfLists: List[List[Int]]): List[List[Int]] = {
    listOfLists match {
      case Nil => List(List())
      case one :: Nil => List(one)
      case one :: two :: Nil => for (o <- one; t <- two) yield o :: t :: Nil
      case head :+ last => for (h <- combos(head); l <- last) yield h :+ l
    }
  }


  "f" should "permutate" in {
    f(listA, listB) should equal(
      List(
        List(1, 3), List(1, 4),
        List(2, 3), List(2, 4)
      ))
  }

  "g" should "permutate" in {
    g(listA, listB, listC) should equal(
      List(
        List(1, 3, 5), List(1, 3, 6),
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      ))
  }


  "combos" should "permutate" in {
    combos(List(listA, listB, listC)) should equal(
      List(
        List(1, 3, 5), List(1, 3, 6),
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      ))
  }
}
