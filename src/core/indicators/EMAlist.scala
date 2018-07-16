package core.indicators

import scala.collection.mutable.ListBuffer

class EMAlist(var number:Int, var list:ListBuffer[Float], var start:Int){
  
  var values = scala.collection.mutable.ListBuffer.empty[Float]
  var candlesL = list
  var sum:Float = 0
  var mul:Float = (2.0/(number+1.0)).toFloat
  
  def UpdateIndicator() = {
    val csInv = candlesL
    for(i <- 0 to number+start-2){
      values.append(0)
      sum += csInv(i)
    }
    //Add first EMA (SMA)
    values.append((sum + csInv(number+start-1))/number)
    
    //Add the EMA's
    for(i <- number+start to candlesL.length-1){  
      values.append((csInv(i) - values(i-1))*mul + values(i-1))
    }
    
  }
  
  def PrintIndicator() = {
    for(e <- values){
      println(e.toString())
    }
  }
  
  def CleanValues(n:Int) = {
    for(i <- 0 to n-1){
      values(i) = 0
    }
  }
  
}