package org.buckleys

import collection.mutable.ArrayBuffer
import math._

object Prime { 
	val primeStore:ArrayBuffer[Long] = ArrayBuffer(2, 3)
	var testIndex = 1

    def apply(n:Int):Long = {
	  //println(n)
	  if (n <= primeStore.length) primeStore(n-1)
	  else for (i <- primeStore.length to n) nextPrime()  
	  primeStore(n-1)
	}
    
	def isPrime(n:Long):Boolean = 
	  //if (primeStore.contains(n)) true else
	  if (n < 2) false 
	  else {
		!stream.takeWhile(p=> p * p <= n).exists(n % _ == 0)
	  }
	
    private def nextPrime() = {
       var candidatePrime = primeStore.last
       do {
         candidatePrime += 2
         if (candidatePrime % 3 == 0) candidatePrime += 2
         var test = this(testIndex)
         while (test * test < candidatePrime) {
           testIndex += 1
           test = this(testIndex)
         }
       } 
       while (primeStore.view(0,testIndex).exists(candidatePrime % _ == 0))
       primeStore.append(candidatePrime);
       candidatePrime
    }
    
    def streamUpTo(n:Long):Stream[Long] = stream.takeWhile(_ <= n)
    
    val stream = streamFrom(1)
    
    def streamFrom(n:Int):Stream[Long] = Prime(n) #:: streamFrom(n + 1)
    
  
    /*
  def primes(): Stream[Int] = {
    def primeStream(s: Stream[Int]): Stream[Int] = s.head #:: primeStream(s.tail filter { _ % s.head != 0 })
    primeStream(Stream.from(2));
  }
    */
    
}