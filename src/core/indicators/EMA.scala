package core.indicators

import core.CandleSeries

class EMA(var number:Int, var cs:CandleSeries){
  
  var values = scala.collection.mutable.ListBuffer.empty[Float]
  var candlesL = cs.CS
  var sum:Float = 0
  var mul:Float = (2.0/(number+1.0)).toFloat
  
  def UpdateIndicator() = {
    val csInv = candlesL
    for(i <- 0 to number-2){
      values.append(0)
      sum += csInv(i).close
    }
    //Add first EMA (SMA)
    values.append((sum + csInv(number-1).close)/number)
    
    //Add the EMA's
    for(i <- number to candlesL.length-1){  
      values.append((csInv(i).close - values(i-1))*mul + values(i-1))
    }
    
  }
  
  def PrintIndicator() = {
    for(e <- values){
      println(e.toString())
    }
  }
  
}