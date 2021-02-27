import org.scalatest.flatspec._
import org.scalatest.matchers._

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  val listA = List(1, 2)
  val listB = List(3, 4)
  val listC = List(5, 6)
  val listD = List(7, 8)

  def f(one: List[Int], two: List[Int]): List[List[Int]] = {
    for (first <- one; second <- two) yield List(first, second)
    //    one.flatMap(
    //      first => two.map(second => List(first, second))
    //    )
  }

  def g(one: List[Int], two: List[Int], three: List[Int]): List[List[Int]] = {
    //    for (first <- one; second <- two; third <- three) yield List(first, second, third)
    one.flatMap(
      first => two.map(second => List(first, second))
    ).flatMap(
      perm => three.map(third => perm :+ third)
    )
  }

  def h(listOfLists: List[List[Int]]): List[List[Int]] = {
    listOfLists match {
      case Nil => List(List())
      case one :: Nil => List(one)
      case one :: two :: Nil => f(one, two)
      case head :+ last=> hPerm(last, h(head))
    }
  }

  def hPerm(three: List[Int], two: List[List[Int]]): List[List[Int]] = {
    two.flatMap(perm => three.map(l => perm :+ l))
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

  "hPerm" should "permutate" in {
    hPerm(listC, f(listA, listB)) should equal(
      List(
        List(1, 3, 5), List(1, 3, 6),
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      ))
  }

  "h" should "permutate" in {
    h(List(listA, listB, listC)) should equal(
      List(
        List(1, 3, 5), List(1, 3, 6),
        List(1, 4, 5), List(1, 4, 6),
        List(2, 3, 5), List(2, 3, 6),
        List(2, 4, 5), List(2, 4, 6)
      ))
  }
}
