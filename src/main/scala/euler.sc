import org.buckleys._
object euler {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Prime.isPrime(9)                                //> res0: Boolean = false
   
  val x = (1 to 5).foldLeft(0)(_ + _)             //> x  : Int = 15
	import org.buckleys._
	Prime.stream().take(5).toList             //> res1: List[Long] = List(2, 3, 5, 7, 11)
 
	//Prime.factors(36288012312L).groupBy(x => x).map({case (k, v) =>  v.size})
    
   '3'.asDigit                                    //> res2: Int = 3

   'C' - 'A' + 1                                  //> res3: Int = 3

  for (i <- 1 to 9; j <- 1 to 9) yield (i, j)     //> res4: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (2,1), (2,2), (2,3), (2,
                                                  //| 4), (2,5), (2,6), (2,7), (2,8), (2,9), (3,1), (3,2), (3,3), (3,4), (3,5), (3
                                                  //| ,6), (3,7), (3,8), (3,9), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7), (
                                                  //| 4,8), (4,9), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (5,7), (5,8), (5,9), 
                                                  //| (6,1), (6,2), (6,3), (6,4), (6,5), (6,6), (6,7), (6,8), (6,9), (7,1), (7,2),
                                                  //|  (7,3), (7,4), (7,5), (7,6), (7,7), (7,8), (7,9), (8,1), (8,2), (8,3), (8,4)
                                                  //| , (8,5), (8,6), (8,7), (8,8), (8,9), (9,1), (9,2), (9,3), (9,4), (9,5), (9,6
                                                  //| ), (9,7), (9,8), (9,9))
}