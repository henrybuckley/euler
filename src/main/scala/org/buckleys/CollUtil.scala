package org.buckleys

object CollUtil {
   def removeFrom[A](s: List[A], e:A) = {
     val (h, t) =  s.span(e != _)
     h ++ (if (t.isEmpty) List() else t.tail)
   }
   
   def removeAllFrom[A](s: List[A], r:Seq[A]) = {
      r.foldLeft(s)(removeFrom(_, _))
   }
}