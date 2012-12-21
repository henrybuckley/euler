package org.buckleys

import collection.mutable.ArrayBuffer
import math._

object Prime { 
	val primeStore:ArrayBuffer[Long] = ArrayBuffer(2, 3)	
	
	private val testPrimes:ArrayBuffer[Long] = ArrayBuffer(2, 3)
	
    def apply(n:Int):Long = {
	  if (n <= primeStore.length) primeStore(n-1)
	  else for (i <- primeStore.length to n) nextPrime()  
	  primeStore(n-1)
	}
    
	def isPrime(n:Long):Boolean = {
	  if (n < 2) false 
	  else {
		val limit = sqrt(n)
		!stream().takeWhile(_ < limit).exists(n % _ == 0)
	  }
	}
	
	private def updateTestPrimesFor(n:Long) {
	  val limit = sqrt(n)
	  
	  if (testPrimes.last < limit) {
		  var next = this(testPrimes.length + 1)
	  
		  while (limit >= next) {
			  testPrimes.append(next)
			  next = this(testPrimes.length + 1)
		  }
	  }
	}
	
    private def nextPrime() {
       var candidatePrime = primeStore.last + 2
       var limit = sqrt(candidatePrime)
       updateTestPrimesFor(candidatePrime)
       
       testPrimes.view(1,3)
       while (testPrimes.exists(candidatePrime % _ == 0)) {
         candidatePrime += 2
         limit = sqrt(candidatePrime)
         updateTestPrimesFor(candidatePrime)
       }

       primeStore.append(candidatePrime);
    }
    
    
    def streamUpTo(n:Long):Stream[Long] = {
      if (n > primeStore.last) {
    	do {
    	  nextPrime()
    	}
        while (n > primeStore.last)
      }
      
      return primeStore.takeWhile(_ <= n).toStream
    }
    
    def stream():Stream[Long] = {
      def stream0(n:Int):Stream[Long] = {
        Prime(n) #:: stream0(n + 1)
      }
      stream0(1)
    }
  
    /*
  def primes(): Stream[Int] = {
    def primeStream(s: Stream[Int]): Stream[Int] = s.head #:: primeStream(s.tail filter { _ % s.head != 0 })
    primeStream(Stream.from(2));
  }
    */
    
 
    
}