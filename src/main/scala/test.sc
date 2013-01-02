
import scala.collection._
import scala.collection.generic._
import org.buckleys._
object test {


  for (i <- Stream.from(1); if i <= 2; j <- Stream.from(1); if j <= 2) yield (i, j)
                                                  //> res0: scala.collection.immutable.Stream[(Int, Int)] = Stream((1,1), ?)
   
  /*
  val m1 = new TruncatableStream(Stream.from(1))
  val m2 = new TruncatableStream(Stream.from(1))
  val m3 = new TruncatableStream(Stream.from(1))
  val pairs = for {i <- m1
                   j <- m2
                   k <- m3
                   if j < 4 || m2.truncate
                   if k < 4 || m3.truncate
                   } yield (i, j, k)
   
    pairs.take(2).toList
    pairs.take(20).toList
   */
    Stream.from(1).map(x => 2 * x * x).takeWhile(_ < 5777).find(x => Prime.isPrime(5777 - x))
                                                  //> res1: Option[Int] = None
                                                  
    Factor.primeFactors(12).groupBy(x => x).values//> res2: Iterable[List[Long]] = MapLike(List(3), List(2, 2))
    
    Factor.primeFactors(134043)                   //> res3: List[Long] = List(3, 7, 13, 491)
    
    BigInt(1) to 10                               //> res4: scala.collection.immutable.NumericRange.Inclusive[BigInt] = NumericRan
                                                  //| ge(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
                                                  
                                                  
    def ispermutation(n1:Long, n2:Long*) = n2.map(_.toString).toSet.subsetOf(n1.toString.permutations.toSet)
                                                  //> ispermutation: (n1: Long, n2: Long*)Boolean
     
    def ispermutation2(n1:Long, n2:Long*) = n2    //> ispermutation2: (n1: Long, n2: Long*)Long*
  
    ispermutation(1234, 4321, 4312)               //> res5: Boolean = true
    
    ispermutation2(1234, 4321, 4312).map(_.toString).toSet.subsetOf(1234.toString.permutations.toSet)
                                                  //> res6: Boolean = true
    1234.toString.permutations.toSet              //> res7: scala.collection.immutable.Set[String] = Set(2431, 3214, 4132, 2143, 
                                                  //| 1243, 2341, 2134, 4312, 3124, 3142, 1432, 1234, 1324, 4213, 2413, 3421, 134
                                                  //| 2, 4321, 4231, 3241, 3412, 4123, 1423, 2314)
                                                  
     // ((1487,4817,8147), ?)
     
    Prime.isPrime(4817)                           //> res8: Boolean = true
     
    //Prime.isPrime(8147)

  List(1,1,2,3).scan(0)(_ + _)                    //> res9: List[Int] = List(0, 1, 2, 4, 7)|

}