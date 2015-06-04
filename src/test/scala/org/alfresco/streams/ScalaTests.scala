package org.alfresco.streams

import org.scalatest.Assertions._

import org.scalatest._
import scala.math.BigInt

/**
 * @author sglover
 */
class ScalaTests extends FunSuite {

  /**
   * Adapted from http://derekwyatt.org/2011/07/29/understanding-scala-streams-through-fibonacci.html
   */
  test("Fibonacci")
  {
    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(
      fibs.tail).map(n => {
        println("Adding %d and %d".format(n._1, n._2))
        n._1 + n._2
      })

      val expected = Array(BigInt(0), BigInt(1), BigInt(1), BigInt(2), BigInt(3), BigInt(5))
      var i = 0
      def ast(x: BigInt): Unit = {
        assert(x == expected(i))
        i += 1
      }

      fibs take 5 foreach ast
  }
  
  test("Currying")
  {
      def f(x:Int)(y:Int) = {
        x * y
      }
      val f1 = f(2)_
      assert(f1(3) == 6)
  }
  
  test("Filtering")
  {
    def filter(xs: List[Int], p: Int => Boolean): List[Int] =
      if (xs.isEmpty) xs
      else if (p(xs.head)) xs.head :: filter(xs.tail, p)
      else filter(xs.tail, p)

    def modN(n: Int)(x: Int) = ((x % n) == 0)
    val nums = List(1, 2, 3, 4, 5, 6, 7, 8)
    println(filter(nums, modN(2)))
    println(filter(nums, modN(3)))
  }
}
