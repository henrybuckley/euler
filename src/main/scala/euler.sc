import org.buckleys._
import MathUtil._
object euler {
  List(3, 4) span (3 !=)                          //> res0: (List[Int], List[Int]) = (List(),List(3, 4))
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   
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
  
  val s = (1 to 10)                               //> s  : scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6, 7,
                                                  //|  8, 9, 10)
  s zip s.tail                                    //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,2), (2,3
                                                  //| ), (3,4), (4,5), (5,6), (6,7), (7,8), (8,9), (9,10))

  (for {i <- Stream.from(2)
       j <- 1 to i - 1} yield (i, j)).take(20).toList
                                                  //> res2: List[(Int, Int)] = List((2,1), (3,1), (3,2), (4,1), (4,2), (4,3), (5,1
                                                  //| ), (5,2), (5,3), (5,4), (6,1), (6,2), (6,3), (6,4), (6,5), (7,1), (7,2), (7,
                                                  //| 3), (7,4), (7,5))
  (1L to 100000L).map(x => (x, x * x)).map({case (x, xsq) => scala.math.abs(Math.sqrt(xsq) - x)}).sum
                                                  //> res3: Double = 0.0

 //  for (i <- Stream.from(1); if i <= 2; j <- Stream.from(1); if j <= 2) yield (i, j)
}