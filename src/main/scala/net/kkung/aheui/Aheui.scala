package net.kkung.aheui
import net.kkung.aheui.parser.AheuiParser
import net.kkung.aheui.parser.AheuiTokenBlank
import net.kkung.aheui.vm._
import net.kkung.aheui.parser.AheuiToken
import io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.{ ByteArrayOutputStream, DataOutputStream, FileOutputStream, FileInputStream, DataInputStream, BufferedInputStream}

object Aheui {

  private def use[T <: { def close(): Unit }](closable: T)(block: T => Unit) {
    try {
      block(closable)
    }
    finally {
      closable.close()
    }
  }

  def main(args: Array[String]): Unit = {

    import java.io.{BufferedOutputStream, PrintStream}
    Console.setOut(new PrintStream(new BufferedOutputStream(new FileOutputStream(java.io.FileDescriptor.out), 512), false, "UTF-8"))

    args(0) match { 

      case "compile" => compile(args(1))
      case "run" => run(args(1))
      case _@cmd => { 
        Console.err.println("Unknow command %s".format(cmd))
        System.exit(1)

      }
    }

    Console.flush
  }

  def compile(file:String) = { 
    val source = Source.fromFile(file, "UTF-8")

    val parser = new AheuiParser    
    val parsed = parser.parseString(source.mkString)

    val bao = new ByteArrayOutputStream
    val dos = new DataOutputStream(bao)

    val tokens = parsed._3.map(_.toArray).toArray

    dos.writeInt(parsed._1)
    dos.writeInt(parsed._2)

    for ( row <- tokens ) { 
      for ( token <- row ) { 
        token.emitByteCode(dos)
      }
      dos.writeByte(0x7F) // row sep
      dos.writeByte(0x7F)
    }

    use(new FileOutputStream("%s.bc".format(file))) { 
      out => { 
        bao.writeTo(out)
        } 
    }

  }

  def run(file:String) = {      
    use(new FileInputStream(file)) {  fis => { 
      use(new DataInputStream(new BufferedInputStream(fis))) { dis => { 
        val width = dis.readInt
        val height = dis.readInt

        val codeSpace: ArrayBuffer[ArrayBuffer[AheuiToken]] = new ArrayBuffer[ArrayBuffer[AheuiToken]]()
        codeSpace += ArrayBuffer[AheuiToken]()

        while (dis.available != 0) { 
          dis.mark(2)
          val readAhead = (dis.readByte(), dis.readByte())
          if (readAhead == (0x7F, 0x7F)) { 
            if (dis.available > 0 ) { 
              codeSpace += ArrayBuffer[AheuiToken]()
            }
          } else { 
            dis.reset
            codeSpace(codeSpace.length-1) += AheuiToken(dis)
          }
        }

        val context = new AheuiContext(codeSpace.map(_.toArray).toArray, width, height)
        val vm = new AheuiMachine
        vm.run(context)

      }}
    }}
  }
}

