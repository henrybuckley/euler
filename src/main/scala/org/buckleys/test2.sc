import org.buckleys.MathUtil._
import org.buckleys._
import scala.math._
object test2 {
  List(1,1,2,3).scan(0)(_ + _)                    //> res0: List[Int] = List(0, 1, 2, 4, 7)
  //(550 to 1 by -1).toList
  
  //var prod = (1.0, 1.0)
  
  //Prime.stream().takeWhile(x => {prod = (prod._2, prod._1 * x); prod._2 <= 1000000}).toList
  //prod
  
  
  digits(1234125).groupBy(x=>x)                   //> res1: scala.collection.immutable.Map[Int,scala.collection.immutable.IndexedS
                                                  //| eq[Int]] = Map(5 -> Vector(5), 1 -> Vector(1, 1), 2 -> Vector(2, 2), 3 -> Ve
                                                  //| ctor(3), 4 -> Vector(4))
                                                  
  "12345".sliding(2).toList                       //> res2: List[String] = List(12, 23, 34, 45)
  
  val t1 = 'a' ^ 'f'                              //> t1  : Int = 7
  val t2 = (t1 ^ 'f').toChar                      //> t2  : Char = a
  
  "the".r.findAllIn("the the the the").length     //> res3: Int = 4
  
  val x = Prime.stream()                          //> x  : Stream[Long] = Stream(2, ?)
  
  x(0)                                            //> res4: Long = 2
  x(0)                                            //> res5: Long = 2
  x(1)                                            //> res6: Long = 3
  x(1)                                            //> res7: Long = 3
  
  Math.pow(125, 1.0/3) % 1.0                      //> res8: Double = 0.0

  val map = collection.mutable.Map[Int, List[Int]]().withDefaultValue(List())
                                                  //> map  : scala.collection.mutable.Map[Int,List[Int]] = Map()
   
   (sqrt(23) + 4) / 7                             //> res9: Double = 1.2565473604732456
    
    
  val list = List("ab", "def", "t")               //> list  : List[java.lang.String] = List(ab, def, t)
  
  (Seq("") /: Seq("ab", "def", "t")) { (acc, s) =>
     s.flatMap(c => acc.map(_ + c))
  }                                               //> res10: Seq[java.lang.String] = Vector(adt, bdt, aet, bet, aft, bft)
  
  val arr = new Array[Int](5)                     //> arr  : Array[Int] = Array(0, 0, 0, 0, 0)
  arr(1) += 2
  arr(1)                                          //> res11: Int = 2
   
  //map(3) = 2 :: map(3)
  //map(3)
 
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