import org.buckleys._
import MathUtil._
object euler {
  println("Welcome to the Scala worksheet")
  
  /*
  Prime.isPrime(9)
   
   
  val x = (1 to 5).foldLeft(0)(_ + _)
	import org.buckleys._
	Prime.stream().take(5).toList
 
	//Prime.factors(36288012312L).groupBy(x => x).map({case (k, v) =>  v.size})
     
   '3'.asDigit

   'C' - 'A' + 1

  for (i <- 1 to 9; j <- 1 to 9) yield (i, j)
  intpow(2, 4)
  
 "abcda" diff "da"
 
  ("123").permutations.toList

  "012345".toInt
  
  val s="abcd"
  s(0)
  
  "123456".substring(1, 4)
  */
  
  val s = (1 to 10)
  s zip s.tail

  (for {i <- Stream.from(2)
       j <- 1 to i - 1} yield (i, j)).take(20).toList
  (1L to 100000L).map(x => (x, x * x)).map({case (x, xsq) => scala.math.abs(Math.sqrt(xsq) - x)}).sum
                                                  
   7.20 % 6
 //  for (i <- Stream.from(1); if i <= 2; j <- Stream.from(1); if j <= 2) yield (i, j)