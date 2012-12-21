package org.buckleys

import Math._
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

  def primeFactors(n: Long): List[Long] = 
    if (n == 1) List() 
    else Prime.stream().find(n % _ == 0) match {
      case Some(factor) => factor :: primeFactors(n / factor)
    }
 
 
  def gcf(v1: Long, v2: Long): Long = {
    val (smaller, larger) = if (v1 < v2) (v1, v2) else (v2, v1)
    if (smaller == 0) larger else gcf(smaller, larger - smaller)
  }

}