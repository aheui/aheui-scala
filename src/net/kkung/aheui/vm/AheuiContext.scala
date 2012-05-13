package net.kkung.aheui.vm
import scala.collection.mutable.Queue
import scala.collection.immutable.HashMap
import net.kkung.aheui.parser.AheuiToken
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
  
  val stacks:HashMap[Char, AheuiStore[Long]] = HashMap('\0' -> new AheuiStack,
      'ㄱ' -> new AheuiStack,
      'ㄴ' -> new AheuiStack,
      'ㄷ' -> new AheuiStack, 
      'ㄹ' -> new AheuiStack, 
      'ㅁ' -> new AheuiStack,
      'ㅂ' -> new AheuiStack,
      'ㅅ' -> new AheuiStack, 
      'ㅈ' -> new AheuiStack, 
      'ㅊ' -> new AheuiStack, 
      'ㅋ' -> new AheuiStack, 
      'ㅌ' -> new AheuiStack, 
      'ㅍ' -> new AheuiStack, 
      'ㄲ' -> new AheuiStack, 
      'ㄳ' -> new AheuiStack, 
      'ㄵ' -> new AheuiStack, 
      'ㄶ' -> new AheuiStack, 
      'ㄺ' -> new AheuiStack, 
      'ㄻ' -> new AheuiStack, 
      'ㄼ' -> new AheuiStack, 
      'ㄽ' -> new AheuiStack, 
      'ㄾ' -> new AheuiStack, 
      'ㄿ' -> new AheuiStack, 
      'ㅀ' -> new AheuiStack, 
      'ㅄ' -> new AheuiStack, 
      'ㅆ' -> new AheuiStack,
      'ㅇ' -> new AheuiQueue
  )
  
  var current_store = stacks('\0')
  
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
  
  def change_store(name:Char):Unit = current_store = stacks(name)
  
  def wrap:Boolean = {
    if (cursor.x >= codes(cursor.y).length) {
      this.move_cursor();
      true
    } else {
      false
    }
  }

}