package net.kkung.aheui.parser
import scala.collection.mutable.ArrayBuffer

case class AheuiToken(command:Char, cursor:Char, argument:Char)

class AheuiParser {

  final val HANGUL_FIRST = Array[Char]('ㄱ','ㄲ','ㄴ','ㄷ','ㄸ','ㄹ','ㅁ','ㅂ','ㅃ','ㅅ','ㅆ','ㅇ','ㅈ','ㅉ','ㅊ','ㅋ','ㅌ','ㅍ','ㅎ')
  final val HANGUL_MIDDLE = Array[Char]('ㅏ','ㅐ','ㅑ','ㅒ','ㅓ','ㅔ','ㅕ','ㅖ','ㅗ','ㅘ','ㅙ','ㅚ','ㅛ','ㅜ','ㅝ','ㅞ','ㅟ','ㅠ','ㅡ','ㅢ','ㅣ')
  final val HANGUL_LAST = Array[Char]('\0','ㄱ','ㄲ','ㄳ','ㄴ','ㄵ','ㄶ','ㄷ','ㄹ','ㄺ','ㄻ','ㄼ','ㄽ','ㄾ','ㄿ','ㅀ','ㅁ','ㅂ','ㅄ','ㅅ','ㅆ','ㅇ','ㅈ','ㅊ','ㅋ','ㅌ','ㅍ','ㅎ')
  
  def splitJamo(c:Char):Tuple3[Char, Char, Char] = {
    
    if ( c < 0xAC00 || c > 0xD7A3) {
      throw new IllegalArgumentException("%c was not Hangul".format(c));
    }
    
    val code = c - 0xAC00
    return ( HANGUL_FIRST(Math.floor(code/28/21).toInt),
        HANGUL_MIDDLE(Math.floor(code/28).toInt % 21),
        HANGUL_LAST(code%28))
  }
  
  def parseString(s:String) = {
   val codeSpace: ArrayBuffer[ArrayBuffer[AheuiToken]] = new ArrayBuffer[ArrayBuffer[AheuiToken]]()
   codeSpace += ArrayBuffer[AheuiToken]()
   
   s.map(c => {
     c match {
       case '\n' => codeSpace += ArrayBuffer[AheuiToken]()
       case _ => {
         val _code = splitJamo(c)
         codeSpace(codeSpace.length-1) +=  new AheuiToken(_code._1, _code._2, _code._3)
       }
     }
   })
   
   codeSpace
  }
}