package net.kkung.aheui
import net.kkung.aheui.parser.AheuiParser
import net.kkung.aheui.vm._
import net.kkung.aheui.parser.AheuiToken
import io.Source

object Aheui {

  def main(args: Array[String]): Unit = {
    
    val parser = new AheuiParser
    val source = Source.fromFile(args(0), "UTF-8")
    
    val parsed = parser.parseString(source.mkString)
	  val context = new AheuiContext(parsed._3.map(_.toArray).toArray, parsed._1, parsed._2)

	  val vm = new AheuiInterpreter
	  vm.run(context)

  }

}


