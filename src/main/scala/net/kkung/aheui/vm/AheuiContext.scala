package net.kkung.aheui.vm
import scala.collection.mutable.Queue
import scala.collection.immutable.HashMap
import net.kkung.aheui.parser.AheuiToken

case class Cursor(var x:Int, var y:Int) {
  def add_delta(delta:Delta) {
    x += delta.x
    y += delta.y
  }
}
case class Delta(x:Int, y:Int)

class AheuiCode(codes: Array[Array[AheuiToken]]) {
  def get_at(cursor:Cursor):AheuiToken = {
    return codes(cursor.y)(cursor.x)
  }
}

class AheuiContext(codes: Array[Array[AheuiToken]], width:Int, height:Int) {

  val code_space:AheuiCode = new AheuiCode(codes)
  
  val stacks:Array[AheuiStore[Long]] = new Array(28)
  for ( i <- (0 to 27) ) { 
    stacks(i) = new AheuiStack[Long]
  }
  stacks(21) = new AheuiQueue[Long]
  
  var current_store = stacks(0)
  
  var cursor:Cursor = Cursor(0, 0)
  var delta:Delta = Delta(0, 0)
  
  def move_cursor():Unit = move_cursor(this.delta)
  
  def move_cursor(delta:Delta):Unit = {
    
    this.delta = delta
    cursor.add_delta(delta)

    if (cursor.y < 0) cursor.y = height - 1
    if (cursor.y >= height) cursor.y = 0

    if (cursor.x < 0) cursor.x = codes(cursor.y).length - 1
    if (cursor.x >= codes(cursor.y).length && delta.x != 0) cursor.x = 0
  }
  
  def token:AheuiToken = {
    return codes(cursor.y)(cursor.x)
  }
  
  def change_store(index:Int):Unit = current_store = stacks(index)
  
  def wrap:Boolean = {
    if (cursor.x >= codes(cursor.y).length) {
      true
    } else {
      false
    }
  }

}
