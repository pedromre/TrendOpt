package core
import scala.collection.mutable.ListBuffer

class RuleUnderThreshold extends Rule {
  //var p1:ListBuffer[Float] = null
  var p1:Array[Float] = null
  var threshold:Float = Float.NaN
  var set:Boolean = false
  

  //p1 cross under threshold
  def setRule(parameter1:ListBuffer[Float], thresholdParam:Float) = {
    p1 = parameter1.toArray
    threshold = thresholdParam
    set = true
  }
  
  
  override def shouldAct(index:Int) : Boolean =  {
    if(!set)println("Rule not set!")
    if(p1(index) < threshold && p1(index -1) >= threshold) return true
    return false
  }
}