package org.buckleys

import math._

object Main extends App {
 
  //problem1  
  
  //var s:Stream[Long] = fib drop 2 //takeWhile(x:Long => x % 2 == 0)

  
  
  def fib:Stream[Long] = {
    def tail(x:Long, y:Long):Stream[Long] = x #:: tail(y, x + y)
    tail(0,1)
  }
  
  def problem1() {
      val v = (1).to(999).filter(x => x % 3 ==0 || x % 5 == 0 ).sum
      println(v)
  }

  def problem2() {
	  val v:Long = (for (i <- fib drop 2 takeWhile(_ < 4000000) if i % 2 == 0) yield i).sum
	  println(v)
  }
}