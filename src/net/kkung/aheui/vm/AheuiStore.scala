package net.kkung.aheui.vm
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

/***
 * 아희용 저장 공간 공통 인터페이스
 */
trait AheuiStore[T] {
  def insert(elem: T)
  def fetch():T  
  def __head:T
  def size:Int
  def swap():Unit
  def dup():Unit
}

class AheuiStack[T] extends Stack[T] with AheuiStore[T] {
  override def insert(elem:T) = push(elem)
  override def fetch():T = pop()
  override def __head:T = top
  override def size:Int = length
  override def swap():Unit = {
    val one = pop()
    val two = pop()
    push(one)
    push(two)
  }
  override def dup():Unit = {
    push(top)
  }
}

class AheuiQueue[T] extends Queue[T] with AheuiStore[T] {
  override def insert(elem:T) = enqueue(elem)
  override def fetch():T = dequeue()
  override def __head:T = front
  override def size:Int = length
  override def swap():Unit = {
    val one = dequeue()
    val two = dequeue()
    val rest = dequeueAll((f) => true).toList
    
    enqueue(two, one)
    rest.foreach(enqueue(_))
  }
  override def dup():Unit = {
    val one = front
    val rest = dequeueAll((f) => true).toList
    
    enqueue(one) 
    rest.foreach(enqueue(_))
  }
}