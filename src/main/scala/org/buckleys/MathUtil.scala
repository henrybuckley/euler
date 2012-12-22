package org.buckleys
import math._

object MathUtil {
  def fib: Stream[BigInt] = {
    def tail(x: BigInt, y: BigInt): Stream[BigInt] = x #:: tail(y, x + y)
    tail(BigInt(0), BigInt(1))
  }
 
  def fact(n:Long):Long = if (n == 0) 1 else n * fact(n - 1)
   
  def square(x: Long): Long = x * x
  
  def digits(n: Long) = n.toString.map(_.asDigit)
}