package org.buckleys

case class ~~(n:Int, val d:Int) extends Ordered[~~] {
	override def toString() = numer + " / " + denom
	
	private val g  = gcd(n, d)
	val numer = n / g
	val denom = d / g
	
	def this(i:Int) = this(i, 1)
	
	def *(that: ~~) = ~~(numer * that.numer, denom * that.denom )
	def /(that: ~~) = ~~(numer * that.denom, denom * that.numer )
	def +(that: ~~) = ~~(numer * that.denom + denom * that.numer, denom * that.denom)
	def -(that: ~~) = ~~(numer * that.denom - denom * that.numer, denom * that.denom)
	
	def *(that: Int) = ~~(numer * that, denom)
	def /(that: Int) = ~~(numer, denom * that)
	def +(that: Int) = ~~(numer + that * denom, denom)
	def -(that: Int) = ~~(numer - that * denom, denom)
	
	private def gcd(a:Int, b:Int):Int = {
	  if (a == 0) b else gcd (b % a, a)
	}
	
	override def equals(that:Any):Boolean = that match {
	  case that: ~~ => numer == that.numer && denom == that.denom
	  case _ => false
	}
	
	override def hashCode: Int = 41 * numer + denom
	
	
	override def compare(that: ~~):Int = {
	  (numer * that.denom).compareTo(that.numer * denom)
	}
}


object ~~ {
  
  class IntToRational(i: Int) {
	  def ~(that:Int) = ~~(i, that)
  }
  
  implicit def intToRational(i: Int) = new IntToRational(i)
  
  implicit def intToRational2(i: Int) = ~~(i, 1)
}