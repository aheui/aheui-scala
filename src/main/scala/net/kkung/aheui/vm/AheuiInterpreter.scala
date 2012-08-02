package net.kkung.aheui.vm
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.ArrayBuffer
import net.kkung.aheui.parser.AheuiToken
import scala.collection.immutable.HashMap
import scala.collection.mutable.Stack
import net.kkung.aheui.parser.AheuiToken
import net.kkung.aheui.parser.AheuiMeanToken
import net.kkung.aheui.parser.AheuiBlankToken
import scala.collection.mutable.Queue

class AheuiInterpreter {

  case object StopPrinter
  private val aheuiPrinter:Actor = actor { 
    loop { 
      react { 
        case StopPrinter => exit()
        case _@p => Console.print(p)
      }
    }
  }

  def run(context:AheuiContext) = {
    var can_next:Boolean = true
    var cycles = 0
    while (can_next) {
      can_next = step(context)
      context.move_cursor()
      cycles += 1
    }
    
    aheuiPrinter ! StopPrinter
    println("\nTotal cycle: %d".format(cycles))
  }
  
  def step(context:AheuiContext):Boolean = {
    
    if ( context.wrap ) {
      return true
    }
    
    context.token match { 
      
      case AheuiBlankToken() => {
        return true
      }
      case AheuiMeanToken(command, cursor, argument, code) => {
        
        context.delta = calc_delta(cursor, context.delta)
        
        if (required_elem(command) > context.current_store.size) {
          context.delta = Delta(-context.delta.x, -context.delta.y)
          return true
        }
        
        command match {
          case 'ㅇ' => {}
          case 'ㅎ' => return false
          case 'ㄷ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2+arg1)
          }
          case 'ㄸ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2*arg1)
          }
          case 'ㅌ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2 - arg1)
          }
          case 'ㄴ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2 / arg1)
          }
          case 'ㄹ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(arg2 % arg1)
          }
          case 'ㅁ' => {
            val arg = context.current_store.fetch()
            argument match {
              case 'ㅇ' => aheuiPrinter ! arg.toInt
              case 'ㅎ' => aheuiPrinter ! arg.toChar
              case _ => {}
            }
          }
          case 'ㅂ' => {
            val add_val = argument match {
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
            
            argument match {
              case 'ㅇ' | 'ㅎ' => {
                context.current_store.insert(readLong())
              }
              case _ => context.current_store.insert(add_val)
            }
          }
          case 'ㅃ' => {
            context.current_store.dup()
          }
          case 'ㅍ' => {
            context.current_store.swap()
          }
          case 'ㅅ' => {
            context.change_store(code%28)
          }
          case 'ㅆ' => {
            context.stacks(argument).insert(context.current_store.fetch())
          }
          case 'ㅈ' => {
            val arg1 = context.current_store.fetch()
            val arg2 = context.current_store.fetch()
            context.current_store.insert(if (arg2 >= arg1) 1 else 0)
          }
          case 'ㅊ' => {
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
  
  def calc_delta(director: Char, current:Delta):Delta = {
    
    director match {
      case 'ㅏ' => Delta(1, 0)
      case 'ㅑ' => Delta(2, 0)
      case 'ㅓ' => Delta(-1, 0)
      case 'ㅕ' => Delta(-2, 0)
      case 'ㅗ' => Delta(0, -1)
      case 'ㅛ' => Delta(0, -2)
      case 'ㅜ' => Delta(0, 1)
      case 'ㅠ' => Delta(0, 2)
      case 'ㅡ' => Delta(current.x, -current.y)
      case 'ㅢ' => Delta(-current.x, -current.y)
      case 'ㅣ' => Delta(-current.x, current.y)
      case _ => current
    }
    
  }

  def required_elem(command:Char):Int = {
    command match {
      case 'ㄷ' | 'ㄸ' | 'ㅌ' | 'ㄴ' | 'ㄹ' | 'ㅍ' | 'ㅈ' => 2
      case 'ㅁ' | 'ㅃ' | 'ㅆ' | 'ㅊ' => 1 
      case _ => 0
    }
  }
}
