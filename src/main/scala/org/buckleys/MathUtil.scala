package org.buckleys
import math._
import Factor._

object MathUtil {
  def fib: Stream[BigInt] = {
    def tail(x: BigInt, y: BigInt): Stream[BigInt] = x #:: tail(y, x + y)
    tail(BigInt(0), BigInt(1))
  }

  def fact(n: Long): Long = if (n == 0) 1 else n * fact(n - 1)

  def digits(n: Any) = n.toString.filter(_.isDigit).map(_.asDigit)
  
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
  def longsFrom(n: Long, step:Long): Stream[Long] = n #:: longsFrom(n + step, step)

  def isPalindrome(n: Any): Boolean = {
    val s = n.toString
    s.reverse == s
  }
  
  val rootMaxLong = sqrt(Long.MaxValue).toLong
  
  def isSquare(n:Long) = {
    /*
    def iter(low:Long, high:Long):Boolean = {
      val mid = (low + high) / 2
      // avoid long overflow
      val midsq = if (mid > rootMaxLong) Long.MaxValue else mid * mid
      //println(n, low, high, mid, midsq)
      if (low > high) false
      else if (midsq == n) true
      else if (midsq > n) iter(low, mid-1)
      else iter(mid + 1, high)
    }
    if (n == Long.MaxValue) throw new ArithmeticException("Can't handle maximum long value")
    if (n==1 || n == 4) true
    else iter(1, n/3)
    */
    val rt = sqrt(n).toLong
    rt * rt == n
  }

  def pentstream(n: Long): Stream[Long] = pentagon(n) #:: pentstream(n + 1)
  
  def triangle(n: Long) = n * (n + 1) / 2
  def square(n: Long) = n * n
  def pentagon(n: Long) = n * (3 * n - 1) / 2
  def hexagon(n: Long) = n * (2 * n - 1)
  def heptagon(n: Long) = n * (5 * n - 3) / 2
  def octagon(n: Long) = n * (3 * n - 2)

  def ntriangle(t: Long) = (-1 + sqrt(1 + 8 * t)) / 2
  def npentagon(p: Long) = (1 + sqrt(1 + 24 * p)) / 6
  def nhexagon(h: Long) = (1 + sqrt(1 + 8 * h)) / 4
  def nsquare(s: Long) = sqrt(s)
  def nheptagon(h: Long) = (3 + sqrt(9 + 40 * h)) / 10
  def noctagon(o: Long) = (2 + sqrt(4 + 12 * o)) / 6

  def isPentagon(n: Long) = pentagon(npentagon(n).toLong) == n
  def isTriangle(n: Long) = triangle(ntriangle(n).toLong) == n
  def isHexagon(n: Long) = isWhole(nhexagon(n))
  def isHeptagon(n: Long) = isWhole(nheptagon(n))
  def isOctagon(n: Long) = isWhole(noctagon(n))

  def isWhole(d: Double) = d % 1 == 0

  def isCube(n: Long, threshold: Double = 1e-7) = {
    val root = pow(n, 1 / 3.0)
    abs(round(root) - root) < threshold
  }

  def sqrtContinuedFracCoeffs(n: Long, useCache:Boolean = false):Stream[Long] = {
    val cache = collection.mutable.ListBuffer[(Long, Long, Long)]()
    def iter(n: Long, base: Long, numer: Long, denomsub: Long): Stream[Long] = {
      if (useCache && cache.contains(base, numer, denomsub)) Stream.empty
      else {
        cache += ((base, numer, denomsub))
        val newnumer = n - denomsub * denomsub
        val newbase = (numer * (sqrt(n) + denomsub) / newnumer).toInt
        val newdenomsub = newbase * newnumer - denomsub * numer
        val g = gcf(newnumer, numer)
        assert(g == numer)
        base #:: iter(n, newbase, newnumer / g, newdenomsub / g)
      }
    }

    val base = sqrt(n).toLong
    iter(n, base, 1, base)
  }
  
  def sqrtContinuedFrac(n:Long):Stream[(BigInt, BigInt)] = {
    val coeffs = sqrtContinuedFracCoeffs(n)
    def iter(i:Int):Stream[(BigInt, BigInt)] = continuedFrac(i, coeffs) #:: iter(i + 1)
    iter(1)
  }
  
  
  def continuedFrac(nterms: Int, coeffs: Stream[Long]): (BigInt, BigInt) =
      (coeffs.take(nterms + 1) :\ (BigInt(0), BigInt(1))) { (coeff, frac) =>
        {
          val (numer, denom) = (coeff * frac._1 + frac._2, frac._1)
          val g = biggcf(numer, denom)
          (numer / g, denom / g)
        }
      }
  
  def ispermutation(n1: Long, n2: Long*) = n2.toSet[Long].map(_.toString).subsetOf(n1.toString.permutations.toSet)
}