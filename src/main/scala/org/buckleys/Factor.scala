package org.buckleys

import Math._
import MathUtil._
import Prime._

object Factor {
  def divisors(n: Long): Set[Long] =
    if (n == 1) Set(1)
    else Prime.stream().find(n % _ == 0) match {
      case Some(factor) => {
        val subdivisors = divisors(n / factor)
        subdivisors ++ subdivisors.map(_ * factor)
      }
    }

  def properDivisors(n: Long): List[Long] =
    if (n < 2) List() else (divisors(n) - n).toList.sorted

  val factorCache = collection.mutable.Map[Long, List[Long]]()
  def primeFactors(n: Long): List[Long] =
    factorCache.getOrElse(n, {
      factorCache(n) =
        if (n == 1) List()
        else Prime.stream().find(n % _ == 0) match {
          case Some(factor) => factor :: primeFactors(n / factor)
        }
      factorCache(n)
    })

  def gcf(v1: Long, v2: Long): Long = {
    val (smaller, larger) = if (v1 < v2) (v1, v2) else (v2, v1)
    if (smaller == 0) larger
    else if (larger % smaller == 0) smaller
    else gcf(smaller, larger - smaller)
  }

  def biggcf(v1: BigInt, v2: BigInt): BigInt = {
    val (smaller, larger) = if (v1 < v2) (v1, v2) else (v2, v1)
    if (smaller == 0) larger
    else if (larger % smaller == 0) smaller
    else biggcf(smaller, larger - smaller)
  }

  //def

}

object Phi {

  var initialised = false
  val MAX_CACHED = 1000000

  val phiarr = (0L to MAX_CACHED).toArray

  def initialisePhi(): Unit = {
    if (initialised) return
    val primes = Prime.stream()
    for {
      i <- 2 to MAX_CACHED
      if (phiarr(i) == i)
      j <- Stream.from(0).map(x => i * (x + 1)).takeWhile(_ <= MAX_CACHED)
    } {
      phiarr(j.toInt) = phiarr(j.toInt) - phiarr(j.toInt) / i
    }

    println("Phi initialized")
    initialised = true
  }

  def phi(n: Long) = {
    if (n <= MAX_CACHED && !initialised) {
      initialisePhi()
      phiarr(n.toInt)
    } else (n /: Factor.primeFactors(n).distinct)
      { (prod, p) => prod * (p - 1) / p }
  }

}

 
  //def phi(n:Long) = 
  