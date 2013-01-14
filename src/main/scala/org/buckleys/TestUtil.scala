package org.buckleys

object TestUtil {
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000000
    println("%d milliseconds".format(micros))
    result
  }
}