package org.buckleys

import collection.mutable.ArrayBuffer
import math._

object Prime { 
	val primeStore:ArrayBuffer[Long] = ArrayBuffer(2, 3)
	var testIndex = 1

    def apply(n:Int):Long = {
	  if (n <= primeStore.length) primeStore(n-1)
	  else for (i <- primeStore.length to n) nextPrime()  
	  primeStore(n-1)
	}
    
	def isPrime(n:Long):Boolean = 
	  if (n < 2) false 
	  else {
		val limit = sqrt(n)
		!stream().takeWhile(_ <= limit).exists(n % _ == 0)
	  }
	
    private def nextPrime() = {
       var candidatePrime = primeStore.last
       do {
         candidatePrime += 2
         val limit = sqrt(candidatePrime) 
         while (this(testIndex) < limit) testIndex += 1
       } 
       while (primeStore.view(0,testIndex).exists(candidatePrime % _ == 0))
       primeStore.append(candidatePrime);
       candidatePrime
    }
    
    def streamUpTo(n:Long):Stream[Long] = stream().takeWhile(_ <= n)
    
    def stream():Stream[Long] = {
      def stream0(n:Int):Stream[Long] = Prime(n) #:: stream0(n + 1)
      stream0(1)
    }
  
    /*
  def primes(): Stream[Int] = {
    def primeStream(s: Stream[Int]): Stream[Int] = s.head #:: primeStream(s.tail filter { _ % s.head != 0 })
    primeStream(Stream.from(2));
  }
    */
    
}