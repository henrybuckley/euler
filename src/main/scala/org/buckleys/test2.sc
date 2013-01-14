import org.buckleys.MathUtil._
import org.buckleys._
import scala.math._
object test2 {


  log(7830457)/log(2)                             //> res0: Double = 22.90066507765709
  BigInt(2).pow(32)                               //> res1: scala.math.BigInt = 4294967296
  
  var lg = BigInt(2)                              //> lg  : scala.math.BigInt = 2
  for (i <- 1 to 5) {
    lg = (lg * lg).mod(1000)
  }
  lg                                              //> res2: scala.math.BigInt = 296
  
  
  35417207621L * 35417207621L                     //> res3: Long(-1343229030247L) = -1343229030247
  3 - 2.3                                         //> res4: Double(0.7000000000000002) = 0.7000000000000002
  1.0 * 2 + 1.0 *2 /3                             //> res5: Double = 2.6666666666666665
  List(1, 2, 3) diff List(3, 4, 5, 6)             //> res6: List[Int] = List(1, 2)
  val comb = List(1, 2, 3).combinations(2)        //> comb  : Iterator[List[Int]] = non-empty iterator
  val pr = for {
    a <- comb
    b <- List(4, 5, 6).combinations(2)
  } yield (a, b)                                  //> pr  : Iterator[(List[Int], List[Int])] = non-empty iterator
  
 // pr.toList.size
  pr.toList                                       //> res7: List[(List[Int], List[Int])] = List((List(1, 2),List(4, 5)), (List(1, 
                                                  //| 2),List(4, 6)), (List(1, 2),List(5, 6)), (List(1, 3),List(4, 5)), (List(1, 3
                                                  //| ),List(4, 6)), (List(1, 3),List(5, 6)), (List(2, 3),List(4, 5)), (List(2, 3)
                                                  //| ,List(4, 6)), (List(2, 3),List(5, 6)))
  (4 + 1) % 5                                     //> res8: Int = 0
 
  List(1,1,2,3).scan(0)(_ + _)                    //> res9: List[Int] = List(0, 1, 2, 4, 7)
  //(550 to 1 by -1).toList
  
  //var prod = (1.0, 1.0)
  
  //Prime.stream().takeWhile(x => {prod = (prod._2, prod._1 * x); prod._2 <= 1000000}).toList
  //prod
  
  
  digits(1234125).groupBy(x=>x)                   //> res10: scala.collection.immutable.Map[Int,scala.collection.immutable.Indexed
                                                  //| Seq[Int]] = Map(5 -> Vector(5), 1 -> Vector(1, 1), 2 -> Vector(2, 2), 3 -> V
                                                  //| ector(3), 4 -> Vector(4))
                                                  
  "12345".sliding(2).toList                       //> res11: List[String] = List(12, 23, 34, 45)
  
  val t1 = 'a' ^ 'f'                              //> t1  : Int = 7
  val t2 = (t1 ^ 'f').toChar                      //> t2  : Char = a
  
  "the".r.findAllIn("the the the the").length     //> res12: Int = 4
  

  
  Math.pow(125, 1.0/3) % 1.0                      //> res13: Double = 0.0

  val map = collection.mutable.Map[Int, List[Int]]().withDefaultValue(List())
                                                  //> map  : scala.collection.mutable.Map[Int,List[Int]] = Map()
   
   (sqrt(23) + 4) / 7                             //> res14: Double = 1.2565473604732456
    
    
  val list = List("ab", "def", "t")               //> list  : List[java.lang.String] = List(ab, def, t)
  
  (Seq("") /: Seq("ab", "def", "t")) { (acc, s) =>
     s.flatMap(c => acc.map(_ + c))
  }                                               //> res15: Seq[java.lang.String] = Vector(adt, bdt, aet, bet, aft, bft)
  
  val arr = new Array[Int](5)                     //> arr  : Array[Int] = Array(0, 0, 0, 0, 0)
  arr(1) += 2
   arr(1)                                         //> res16: Int = 2
  
  BigDecimal(1,101)                               //> res17: scala.math.BigDecimal = 1E-101
  val bd=BigDecimal(1)                            //> bd  : scala.math.BigDecimal = 1
  bd/3.0                                          //> res18: scala.math.BigDecimal = 0.3333333333333333333333333333333333
  
  
  (5 to  1).toList                                //> res19: List[Int] = List()
  
  import collection.mutable.Buffer
  val z = Buffer.fill(2, 2)(2)                    //> z  : scala.collection.mutable.Buffer[scala.collection.mutable.Buffer[Int]] 
                                                  //| = ArrayBuffer(ArrayBuffer(2, 2), ArrayBuffer(2, 2))
                                                  
  val z2 = z.toList.map(_.toList)                 //> z2  : List[List[Int]] = List(List(2, 2), List(2, 2))
                                                  
  z(1)(1) = 3
  z2                                              //> res20: List[List[Int]] = List(List(2, 2), List(2, 2))
             
  sqrt(25 + (30.0/8) * (30.0/8))                  //> res21: Double = 6.25
  sqrt( (6.0 - 30.0/8) * (6.0 - 30.0/8) + 9 )     //> res22: Double = 3.75
  //map(3) = 2 :: map(3)
  //map(3)
  
  6.0 - 30.0/8                                    //> res23: Double = 2.25
 List(3,5,6).permutations.map({case List(a,b,c) => (a,b,c, 1.0 *a * b / (a + c))}).toList
                                                  //> res24: List[(Int, Int, Int, Double)] = List((3,5,6,1.6666666666666667), (3,
                                                  //| 6,5,2.25), (5,3,6,1.3636363636363635), (5,6,3,3.75), (6,3,5,1.6363636363636
                                                  //| 365), (6,5,3,3.3333333333333335))
 
 sqrt(720)                                        //> res25: Double = 26.832815729997478
 
 "test".sliding(2).toList                         //> res26: List[String] = List(te, es, st)
 
 "a" * 3                                          //> res27: String = aaa
  
 // {1 to 100}.map(x => (x, digits(BigInt(9).pow(x)).size)).foreach(println)
  /*
  var ilive, jlive = true
   pow(8, 1.0/3)
  
  
  val pairs = for {
    i <- Stream.from(1).takeWhile(x => {println("takeWhilei: " + x, ilive); /*(x < 100) &&*/ ilive})
    _ = {jlive = true}
    _ = println(i, ilive, jlive)
    j <- Stream.from(1).takeWhile(x => {println("takeWhilej: " + x, jlive); /*(x < 100) &&*/ jlive})
    _ = println(i,j,1,ilive, jlive)
    if ({println("i test: " + i); i < 3 || {println("ilive false"); ilive = false; false}})
    _ = println(i,j,2,ilive, jlive)
    if ({println("j test: " + j); j < 3 || {println("jlive false");jlive = false; false}})
    _ = println(i,j,3,ilive, jlive)
  } yield (i, j)
     
  pairs.take(4).toList
  
  'a' to 'z'
  */
}