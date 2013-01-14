package org.buckleys

import Math._
import MathUtil._
import Prime._

object Factor {
  val primes = Prime.stream

  def divisors(n: Long): Set[Long] = {
    assert(n >= 1)
    if (n == 1) Set(1)
    else primes.find(n % _ == 0) match {
      case Some(factor) => {
        val subdivisors = divisors(n / factor)
        subdivisors ++ subdivisors.map(_ * factor)
      }
    }
  }

  def properDivisors(n: Long): List[Long] =
    (divisors(n) - n).toList.sorted

  val factorCache = collection.mutable.Map[Long, List[Long]]()

  def sumdiv(n: Long) = {
    //properDivisors(n).sum 
    var sum = 1l
    var lastsum = 0l
    var ndiv = n
    for {
      p <- primes.takeWhile(p => p * p < n)
    } {
      lastsum = sum
      while (ndiv % p == 0) {
        ndiv = ndiv / p
        sum = sum * p + lastsum
      }
    }
    if (ndiv > 1) sum *= (ndiv + 1)
    sum
  }
  
  def sumpropdiv(n:Long) = sumdiv(n) - n

  def primeFactors(n: Long): List[Long] =
    factorCache.getOrElse(n, {
      factorCache(n) =
        if (n == 1) List()
        else primes.find(n % _ == 0) match {
          case Some(factor) => factor :: primeFactors(n / factor)
        }
      factorCache(n)
    })

  def factorproducts(n: Long): List[List[Long]] = {
    def iter(factors: List[Long]): List[List[Long]] = {
      if (factors.isEmpty) List(List())
      else (for {
        l <- (1 to factors.length).toList
        comb <- factors.combinations(l).toList.distinct
        subFactors <- iter(factors diff comb)
      } yield { comb.product :: subFactors }).map(_.sorted).distinct
    }

    iter(Factor.primeFactors(n))
  }

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
    } else (n /: Factor.primeFactors(n).distinct) { (prod, p) => prod * (p - 1) / p }
  }

}

 
  //def phi(n:Long) = 
  