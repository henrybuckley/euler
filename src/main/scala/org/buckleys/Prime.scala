package org.buckleys

import collection.mutable.ListBuffer
import math._

object Prime {
	val primeStore:ListBuffer[Long] = ListBuffer(2, 3)
	
	private val testPrimes:ListBuffer[Long] = ListBuffer(2, 3)
	
    def apply(n:Int):Long = {
	  if (n <= primeStore.length) 
	    return primeStore(n-1)
	  else
	    for (i <- primeStore.length to n) {
	      nextPrime()
	    }
	    return primeStore(n-1)
	}
    
	private def updateTestPrimesFor(n:Long) {
	  val limit = sqrt(n)
	  
	  if (testPrimes.last < limit) {
		  var next = this(testPrimes.length + 1)
	  
		  while (limit > next) {
			  testPrimes.append(next)
			  next = this(testPrimes.length + 1)
		  }
	  }
	}
	
    private def nextPrime() {
       var candidatePrime = primeStore.last + 2
       var limit = sqrt(candidatePrime)
       updateTestPrimesFor(candidatePrime)
       
       while (testPrimes.exists(candidatePrime % _ == 0)) {
         candidatePrime += 2
         limit = sqrt(candidatePrime)
         updateTestPrimesFor(candidatePrime)
       }

       primeStore.append(candidatePrime);
       if (primeStore.length % 1000 == 0) {
         println(candidatePrime)
       }
    }
    
    def streamUpTo(n:Long):Stream[Long] = {
      if (n > primeStore.last) {
    	do {
    	  nextPrime()
    	}
        while (n > primeStore.last)
        primeStore.remove(primeStore.length - 1)
        return primeStore.toStream
      }
      
      return primeStore.takeWhile(_ <= n).toStream
    }
    
}