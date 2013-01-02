package org.buckleys

import scala.collection._
import scala.collection.generic._

class TruncatableStream[A](private val underlying: Stream[A]) {
  private var isTruncated = false;

  private var active = underlying.takeWhile(a => !isTruncated)

  def flatMap[B, That](f: (A) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = active.flatMap(f);

  def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = active.map(f);

  def filter(p: A => Boolean): Stream[A] = active.filter(p);

  
 // def foldLeft[B](z: B)(op: (B, A) => B): B = active.foldLeft(z)(op)
  
  
  def truncate() = {
    isTruncated = true
    false
  }
  
  def reset() = {
    isTruncated = false
     
    active = underlying.takeWhile(a => !isTruncated) 
  }
 }

