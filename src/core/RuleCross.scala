package core

import scala.collection.mutable.ListBuffer


class RuleCross(var name:String=null) extends Rule {
  //var p1:ListBuffer[Float] = null
  //var p2:ListBuffer[Float] = null
  var p1:Array[Float] = null
  var p2:Array[Float] = null
  var act:String = null
  var set:Boolean = false
  

  //p1 cross over p2
  def setRule(parameter1:ListBuffer[Float], action:String, parameter2:ListBuffer[Float]) = {
    p1 = parameter1.toArray
    p2 = parameter2.toArray
    set = true
  }
  
  
  override def shouldAct(index:Int) : Boolean =  {
    if(!set)println("Rule not set!")
    if(p1(index) > p2(index) && p1(index -1) <= p2(index-1)) return true
    return false
  }

}

