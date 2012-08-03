package net.kkung.aheui.parser

import scala.collection.mutable.HashMap
import scala.math.ScalaNumber

class AheuiToken(delta:AheuiDelta) { 
  
  def dt = delta
  def emitByteCode(writer:java.io.DataOutputStream) = { 
    writer.writeByte(getOpCode)
    delta.emitByteCode(writer)
  }

  private def getOpCode():Byte = { 
    this.getClass.getAnnotations.filter((a) => {
      a.isInstanceOf[AheuiOpcode]
    }).head.asInstanceOf[AheuiOpcode].value
  }
}

@AheuiOpcode(0x10)
case class AheuiTokenNop(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x11)
case class AheuiTokenExit(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x12)
case class AheuiTokenAdd(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x13)
case class AheuiTokenMul(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x14)
case class AheuiTokenSub(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x15)
case class AheuiTokenDiv(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x16)
case class AheuiTokenMod(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x17)
case class AheuiTokenPop(delta:AheuiDelta, asInt:Option[Boolean]) extends AheuiToken(delta) { 

  override def emitByteCode(writer:java.io.DataOutputStream) = { 
    super.emitByteCode(writer)
    asInt match { 
      case Some(true) => writer.writeByte(AheuiTokenPop.POP_AS_INT)
      case Some(false) => writer.writeByte(AheuiTokenPop.POP_AS_CHAR)
      case _ => writer.writeByte(AheuiTokenPop.POP_DISCARD)
    }
  }

}

object AheuiTokenPop { 

  private [AheuiTokenPop] final val POP_AS_INT = 1
  private [AheuiTokenPop] final val POP_AS_CHAR = 2
  private [AheuiTokenPop] final val POP_DISCARD = 9
  
  def apply(delta:AheuiDelta, asInt:Byte): AheuiTokenPop = { 

    new AheuiTokenPop(delta, asInt match { 
      case POP_AS_INT => Some(true)
      case POP_AS_CHAR => Some(false)
      case _ => None
    })

  }

}


@AheuiOpcode(0x18)
case class AheuiTokenPush(delta:AheuiDelta, fromStdIn:Boolean, asInt:Option[Boolean], value:Option[Byte]) extends AheuiToken(delta) { 

  override def emitByteCode(writer:java.io.DataOutputStream) = { 
    super.emitByteCode(writer)
    writer.writeByte(if (fromStdIn == true) 1 else 0)
    writer.writeByte(if (asInt.getOrElse(false) == true ) 1 else 0)
    writer.writeByte(value.getOrElse(0x7E).asInstanceOf[Byte])
  }

}

object AheuiTokenPush { 

  def apply(delta:AheuiDelta, fromStdIn:Byte, asInt:Byte, value:Byte):AheuiTokenPush = { 
    new AheuiTokenPush(delta, 
                       if (fromStdIn == 1) true else false,
                       if (asInt == 1) Some(true) else Some(false),
                       if (value == 0x7E) None else Some(value))
  }

}

@AheuiOpcode(0x19)
case class AheuiTokenDup(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x1a)
case class AheuiTokenSwap(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x1b)
case class AheuiTokenSel(delta:AheuiDelta, index:Byte) extends AheuiToken(delta) { 

  override def emitByteCode(writer:java.io.DataOutputStream) = { 
    super.emitByteCode(writer)
    writer.writeByte(index)
  }

}

@AheuiOpcode(0x1c)
case class AheuiTokenMov(delta:AheuiDelta, index:Byte) extends AheuiToken(delta) { 

  override def emitByteCode(writer:java.io.DataOutputStream) = { 
    super.emitByteCode(writer)
    writer.writeByte(index)
  }

}

@AheuiOpcode(0x1d)
case class AheuiTokenCmp(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x1e)
case class AheuiTokenCond(delta:AheuiDelta) extends AheuiToken(delta)

@AheuiOpcode(0x1f)
case class AheuiTokenBlank() extends AheuiToken(new AheuiDeltaKeep)

class AheuiDelta() { 

  private def getOpCode():Byte = { 
    this.getClass.getAnnotations.filter((a) => {
      a.isInstanceOf[AheuiOpcode]
    }).head.asInstanceOf[AheuiOpcode].value
  }

  def emitByteCode(writer:java.io.DataOutputStream) = { 
    writer.writeByte(getOpCode)
  }
}

@AheuiOpcode(0x20) case class AheuiDeltaKeep() extends AheuiDelta
@AheuiOpcode(0x21) case class AheuiDeltaRight() extends AheuiDelta
@AheuiOpcode(0x22) case class AheuiDeltaRightRight() extends AheuiDelta
@AheuiOpcode(0x23) case class AheuiDeltaLeft() extends AheuiDelta
@AheuiOpcode(0x24) case class AheuiDeltaLeftLeft() extends AheuiDelta
@AheuiOpcode(0x25) case class AheuiDeltaUp() extends AheuiDelta
@AheuiOpcode(0x26) case class AheuiDeltaUpUp() extends AheuiDelta
@AheuiOpcode(0x27) case class AheuiDeltaDown() extends AheuiDelta
@AheuiOpcode(0x28) case class AheuiDeltaDownDown() extends AheuiDelta
@AheuiOpcode(0x29) case class AheuiDeltaReverseH() extends AheuiDelta
@AheuiOpcode(0x2a) case class AheuiDeltaReverseV() extends AheuiDelta
@AheuiOpcode(0x2b) case class AheuiDeltaReverse() extends AheuiDelta

trait KlassMap[K, T] { 

  val klassMap:HashMap[K, Class[_ <: T]] = HashMap.empty
  
  def regist(k:K, v:Class[_ <:T]) = { 
    klassMap(k) = v
  }
  
  def newInstance[T](args: Array[AnyRef])(implicit m:Manifest[T]): T = { 
    val constructor = m.erasure.getDeclaredConstructors.head
    constructor.newInstance(args: _*).asInstanceOf[T]
  }

  def lookup(k:K, args:Array[AnyRef]) = { 
    val klass = klassMap(k)
    newInstance(args)
  }

  def createInstance(k:K):T = { 
    klassMap(k).newInstance().asInstanceOf[T]
  }


  private def unwrapArg(arg: Any):AnyRef = arg match { 
    case x:ScalaNumber => x.underlying
    case x => x.asInstanceOf[AnyRef]
  }

  def createInstance(k:K, args: Any*):T = { 
    val klass = klassMap(k)
    klass.getDeclaredConstructors.head.newInstance(args map unwrapArg: _*).asInstanceOf[T]
  }

  private [KlassMap] def registAllTargets():Unit = { 

    val packageArray = classOf[AheuiToken].getName.split("\\.").dropRight(1)
    val packageName = packageArray.mkString(".")
    val packagePath = packageArray.mkString("/")
    val classLoader:ClassLoader = this.getClass.getClassLoader
    val resources = classLoader.getResources(packagePath)
    val paths = collection.mutable.ArrayBuffer[java.io.File]()

    while ( resources.hasMoreElements ) { 
      paths.append(new java.io.File(resources.nextElement.getFile))
    }

    for ( path <- paths ) { 
      for ( klasses <- path.listFiles; 
            if klasses.getName.endsWith(".class")) { 
        val klass = Class.forName("%s.%s".format(packageName, klasses.getName.stripSuffix(".class")))
        if (klass.isAnnotationPresent(classOf[AheuiOpcode])) { 
          val annotation = klass.getAnnotations().filter((a) => { 
            a.isInstanceOf[AheuiOpcode]
          }).head.asInstanceOf[AheuiOpcode]
          regist(annotation.value.asInstanceOf[K], klass.asInstanceOf[Class[_ <: T]])
        }
      }
    }
  }
  
  registAllTargets()

}


object AheuiDelta extends KlassMap[Byte, AheuiDelta] { 

  def apply(op:Char):AheuiDelta = { 
    op match {
      case 'ㅏ' => new AheuiDeltaRight
      case 'ㅑ' => new AheuiDeltaRightRight
      case 'ㅓ' => new AheuiDeltaLeft
      case 'ㅕ' => new AheuiDeltaLeftLeft
      case 'ㅗ' => new AheuiDeltaUp
      case 'ㅛ' => new AheuiDeltaUpUp
      case 'ㅜ' => new AheuiDeltaDown
      case 'ㅠ' => new AheuiDeltaDownDown
      case 'ㅡ' => new AheuiDeltaReverseH
      case 'ㅢ' => new AheuiDeltaReverse
      case 'ㅣ' => new AheuiDeltaReverseV
      case _ => new AheuiDeltaKeep
    }
  }

  def apply(op:Byte):AheuiDelta = { 
    new AheuiDeltaKeep
  }

}

object AheuiToken extends KlassMap[Byte, AheuiToken] { 

  def apply(parsed: ParsedHangul):AheuiToken = { 
    val delta = AheuiDelta(parsed.middle)
    val token = parsed.first match { 
      case 'ㅇ' => { new AheuiTokenNop(delta) }
      case 'ㅎ' => { new AheuiTokenExit(delta) }
      case 'ㄷ' => { new AheuiTokenAdd(delta) }
      case 'ㄸ' => { new AheuiTokenMul(delta) }
      case 'ㅌ' => { new AheuiTokenSub(delta) }
      case 'ㄴ' => { new AheuiTokenDiv(delta) }
      case 'ㄹ' => { new AheuiTokenMod(delta) }
      case 'ㅁ' => { 
        parsed.last match { 
          case 'ㅇ' => new AheuiTokenPop(delta, Some(true))
          case 'ㅎ' => new AheuiTokenPop(delta, Some(false))
          case _ => new AheuiTokenPop(delta, None)
        }
      }
      case 'ㅂ' => { 
        parsed.last match {
          case 'ㅇ' => new AheuiTokenPush(delta, true, Some(true), None)
          case 'ㅎ' => new AheuiTokenPush(delta, true, Some(false), None)
          case _@argument => { 
            val value = argument match { 
              case 'ㄱ' | 'ㄴ' | 'ㅅ' => 2
              case 'ㄷ' | 'ㅈ' | 'ㅋ' => 3
              case 'ㅁ' | 'ㅂ' | 'ㅊ' | 'ㅌ' | 'ㅍ' | 'ㄲ' | 'ㄳ' | 'ㅆ' => 4
              case 'ㄹ' | 'ㄵ' | 'ㄶ' => 5
              case 'ㅄ' => 6
              case 'ㄺ' | 'ㄽ' => 7
              case 'ㅀ' => 8
              case 'ㄻ' | 'ㄼ' | 'ㄾ' | 'ㄿ' => 9
              case _ => 0
            }
            new AheuiTokenPush(delta, false, None, Some(value.asInstanceOf[Byte]))
          }
        }
      }
      case 'ㅃ' => { new AheuiTokenDup(delta) }
      case 'ㅍ' => { new AheuiTokenSwap(delta) }
      case 'ㅅ' => { new AheuiTokenSel(delta, (parsed.code%28).asInstanceOf[Byte]) }
      case 'ㅆ' => { new AheuiTokenMov(delta, (parsed.code%28).asInstanceOf[Byte]) }
      case 'ㅈ' => { new AheuiTokenCmp(delta) }
      case 'ㅊ' => { new AheuiTokenCond(delta) }
      case _ => {  new AheuiTokenBlank() }
    }

    token
  }

  def apply(reader:java.io.DataInputStream):AheuiToken = { 
    val op = reader.readByte()
    val delta = AheuiDelta.createInstance(reader.readByte())

    op match { 
      case 0x17 => AheuiTokenPop(delta, reader.readByte)
      case 0x18 => AheuiTokenPush(delta, reader.readByte, reader.readByte, reader.readByte)
      case 0x1b | 0x1c => AheuiToken.createInstance(op, delta, reader.readByte)
      case 0x1f => new AheuiTokenBlank
      case x => AheuiToken.createInstance(x, delta)
    }
  }  
}
