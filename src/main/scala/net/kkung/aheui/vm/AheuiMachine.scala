package net.kkung.aheui.vm
import scala.collection.mutable.ArrayBuffer
import net.kkung.aheui.parser.AheuiToken
import scala.collection.immutable.HashMap
import scala.collection.mutable.Stack
import net.kkung.aheui.parser._
import scala.collection.mutable.Queue

class AheuiMachine {

  def run(context:AheuiContext) = {
    var can_next:Boolean = true
    var cycles = 0
    while (can_next) {
      can_next = step(context)
      context.move_cursor()
      cycles += 1
    }
    
    println("\nTotal cycle: %d".format(cycles))
  }
  
  def step(context:AheuiContext):Boolean = {
    
    if ( context.wrap ) {
      return true
    }

    context.token match { 
      
      case AheuiTokenBlank() => { return true }
      case token:AheuiToken => {
  
        context.delta = calc_delta(token.dt, context.delta)
        
        if (required_elem(token) > context.current_store.size) {
          context.delta = Delta(-context.delta.x, -context.delta.y)
          return true
        }      
  
        token match {
          case AheuiTokenNop(_) => { }
          case AheuiTokenExit(_) => { return false }
          case AheuiTokenAdd(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2+arg1)      
          } 
          case AheuiTokenMul(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2*arg1)              
          }
          case AheuiTokenSub(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2-arg1)      
          }
          case AheuiTokenDiv(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2/arg1)      
          }
          case AheuiTokenMod(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2%arg1)      
          }  
          case AheuiTokenPop(_, asInt) => { 
            val arg = context.current_store.fetch()
            asInt match { 
              case Some(true) => print(arg.toInt)
              case Some(false) => print(arg.toChar)
              case _ => { }
            }
          }
          case AheuiTokenPush(_, fromStdIn, asInt, value) => { 
            fromStdIn match { 
              case true => { 
                context.current_store.insert(readLong())
              }
              case false => { 
                context.current_store.insert(value.get)
              }
            }
          }
          case AheuiTokenDup(_) => { 
            context.current_store.dup()
          }
          case AheuiTokenSwap(_) => { 
            context.current_store.swap()
          }
          case AheuiTokenSel(_, index) => { 
            context.change_store(index)
          }
          case AheuiTokenMov(_, index) => { 
            context.stacks(index).insert(context.current_store.fetch())          
          }
          case AheuiTokenCmp(_) => { 
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(if (arg2 >= arg1) 1 else 0)            
          }
          case AheuiTokenCond(_) => { 
            val arg1 = context.current_store.fetch()
            if (arg1 == 0) {
              context.delta = Delta(-context.delta.x, -context.delta.y)
              return true
            }
          }
          case _ => {}
        }    
      }
    }
    return true
  }

  final val DELTA_RIGHT = Delta(1, 0)
  final val DELTA_RIGHTRIGHT = Delta(2, 0)
  final val DELTA_LEFT = Delta(-1, 0)
  final val DELTA_LEFTLEFT = Delta(-2, 0)
  final val DELTA_UP = Delta(0, -1)
  final val DELTA_UPUP = Delta(0, -2)
  final val DELTA_DOWN = Delta(0, 1)
  final val DELTA_DOWNDOWN = Delta(0, 2)
  
  def calc_delta(delta:AheuiDelta, current:Delta):Delta = {
    
    delta match {
      case AheuiDeltaRight() => DELTA_RIGHT
      case AheuiDeltaRightRight() => DELTA_RIGHTRIGHT
      case AheuiDeltaLeft() => DELTA_LEFT
      case AheuiDeltaLeftLeft() => DELTA_LEFTLEFT
      case AheuiDeltaUp() => DELTA_UP
      case AheuiDeltaUpUp() => DELTA_UPUP
      case AheuiDeltaDown() => DELTA_DOWN
      case AheuiDeltaDownDown() => DELTA_DOWNDOWN
      case AheuiDeltaReverseH() => Delta(current.x, -current.y)
      case AheuiDeltaReverse() => Delta(-current.x, -current.y)
      case AheuiDeltaReverseV() => Delta(-current.x, current.y)
      case AheuiDeltaKeep() => current
      case _ => current // ???
    }
    
  }

  def required_elem(token:AheuiToken):Int = {
    token match {
      case AheuiTokenAdd(_) | AheuiTokenMul(_) | AheuiTokenSub(_) | AheuiTokenDiv(_) | AheuiTokenMod(_) | AheuiTokenSwap(_) | AheuiTokenCmp(_) => 2
      case AheuiTokenPop(_, _) | AheuiTokenDup(_) | AheuiTokenMov(_, _) | AheuiTokenCond(_) => 1 
      case _ => 0
    }
  }
}
