package org.buckleys
import math._

object MathUtil {
  def fib: Stream[BigInt] = {
    def tail(x: BigInt, y: BigInt): Stream[BigInt] = x #:: tail(y, x + y)
    tail(BigInt(0), BigInt(1))
  }

  def fact(n: Long): Long = if (n == 0) 1 else n * fact(n - 1)

  def digits(n: Any) = n.toString.map(_.asDigit)

  def intpow(n: Long, e: Int): Long =
    if (e < 0) 0L
    else if (e == 0) 1L
    else List.fill(e - 1)(n).foldLeft(n)(_ * _)

  def bigfact(n: BigInt): BigInt = {
    if (n == 0) BigInt(1) else n * bigfact(n - 1)
  }

  def bigfactrange(ns: Int, ne: Int): BigInt = {
    if (ns == ne) BigInt(1) else ne * bigfactrange(ns, ne - 1)
  }

  def ncr(n: Int, r: Int) = {
    bigfactrange(n - r, n) / bigfact(r)
  }

  def longsFrom(n: Long): Stream[Long] = n #:: longsFrom(n + 1)

  def isPalindrome(n: Any): Boolean = {
    val s = n.toString
    s.reverse == s
  }
  
  def triangle(n: Long) = n * (n + 1) / 2
  def square(n: Long) = n * n
  def pentagon(n: Long) = n * (3 * n - 1) / 2
  def hexagon(n: Long) = n * (2 * n - 1)
  def heptagon(n:Long) = n * (5 * n - 3) / 2
  def octagon(n:Long) = n * (3 * n - 2)
  
  def ntriangle(t: Long) = (-1 + sqrt(1 + 8 * t)) / 2
  def npentagon(p: Long) = (1 + sqrt(1 + 24 * p)) / 6
  def nhexagon(h: Long) = (1 + sqrt(1 + 8 * h)) / 4
  def nsquare(s: Long) = sqrt(s)
  def nheptagon(h: Long) = (3 + sqrt(9 + 40 * h)) / 10
  def noctagon(o: Long) = (2 + sqrt(4 + 12 * o)) / 6
  
  def isPentagon(n: Long) = isWhole(npentagon(n))
  def isSquare(n: Long) = isWhole(nsquare(n))
  def isTriangle(n: Long) = isWhole(ntriangle(n))
  def isHexagon(n: Long) = isWhole(nhexagon(n))
  def isHeptagon(n: Long) = isWhole(nheptagon(n))
  def isOctagon(n: Long) = isWhole(noctagon(n))
  
  def isWhole(d:Double) = d % 1 == 0  
  
  def isCube(n:Long, threshold:Double = 1e-7) = {
    val root = pow(n, 1/3.0)
    abs(round(root) - root) < threshold
  }
}