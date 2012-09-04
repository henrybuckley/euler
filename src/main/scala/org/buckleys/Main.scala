package org.buckleys

import math._
import scala.collection.mutable.ListBuffer

object Main extends App { 
  

  
  var sum:Long = 0  
  val primeStream = Prime.streamUpTo(2000000)
  
  primeStream.foreach {x =>
    sum += x
  }
  println(sum)
  
  //primes.take(5) foreach { println(_) } 
   
  def problem9() {
     
	val tuples = (for (x <- 1 to 1000; y <- 1 to 1000) yield (x, y, sqrt(square(x) + square(y)))) filter (_._3 % 1 == 0)
	val tuple = tuples.find(x => x._1 + x._2 + x._3 == 1000) match {
    	case Some(x) => x
	}
    print(tuple, tuple._1 * tuple._2 * tuple._3.toLong) 
  }
 
  
  def problem8() {   
    val s =  """73167176531330624919225119674426574742355349194934
				96983520312774506326239578318016984801869478851843
				85861560789112949495459501737958331952853208805511
				12540698747158523863050715693290963295227443043557
				66896648950445244523161731856403098711121722383113
				62229893423380308135336276614282806444486645238749
				30358907296290491560440772390713810515859307960866
				70172427121883998797908792274921901699720888093776
				65727333001053367881220235421809751254540594752243
				52584907711670556013604839586446706324415722155397
				53697817977846174064955149290862569321978468622482
				83972241375657056057490261407972968652414535100474
				82166370484403199890008895243450658541227588666881
				16427171479924442928230863465674813919123162824586
				17866458359124566529476545682848912883142607690042
				24219022671055626321111109370544217506941658960408
				07198403850962455444362981230987879927244284909188
				84580156166097919133875499200524063689912560717606
				05886116467109405077541002256983155200055935729725
				71636269561882670428252483600823257530420752963450""".filter(_.isDigit)			
				
    var n = 0
    for (i <- 0 to s.length - 1) {
    val ss = s.substring(i,min(s.length, i + 5))
      n = max(n, ss.map( _.asDigit).reduceLeft(_ * _))
  	}
  	println(n)
  }
  
  def problem7() {
     println(primes()(10000))
  }
  
  def problem6() {
	val sumsquare = (1 to 100).map( x => x * x).sum
	val sum = (1 to 100).sum
	println (sum * sum - sumsquare)
  }
  
  def problem5() {
    var n:Long = 1
    for (i <- 2 to 20 if n % i > 0) 
    	n *= i / gcf(n, i)
    println(n)
  }
  
  def problem4() {
	val palindromes = for (i <- Range(100,999);
						   j <- Range(100,999) if isPalindrome(i * j) ) yield i * j
    println(palindromes.max)
  }
 
  def problem3() {
    val n:Long = 600851475143L
    for (i <- primes takeWhile(_ < sqrt(n)) if n % i == 0)
    	println(i)
  }
    
  def problem2() {
	  val v:Long = (for (i <- fib drop 2 takeWhile(_ < 4000000) if i % 2 == 0) yield i).sum
	  println(v)
  }
  
    
  def problem1() {
      val v = 1.to(999).filter(x => x % 3 ==0 || x % 5 == 0 ).sum
      println(v)
  }


  
  // helper functions
  
  def square(x:Long):Long = x * x
  
  
  def gcf(v1:Long,v2:Long):Long = {
    val smaller = min(v1, v2)
    val larger = max(v1, v2)
    return smaller match {
      case 0 => larger
      case _ => gcf(smaller, larger - smaller) 
    }
  }
  
  def isPalindrome(n:Long):Boolean = {
    val s = n.toString
    s.reverse == s
  } 
 

  def primes() : Stream[Int] = {
    def primeStream(s:Stream[Int]):Stream[Int] = s.head #:: primeStream(s.tail filter { _ % s.head != 0} )
    primeStream(Stream.from(2));
  }
  
   
  
  def fib:Stream[Long] = {
    def tail(x:Long, y:Long):Stream[Long] = x #:: tail(y, x + y)
    tail(0,1)
  }
}