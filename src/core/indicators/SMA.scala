package core.indicators

import core.CandleSeries

class SMA(var number:Int, var cs:CandleSeries){
  
  var values = scala.collection.mutable.ListBuffer.empty[Float]
  var candlesL = cs.CS
  var sum:Float = 0
  
  def UpdateIndicator() = {
    val csInv = candlesL
    for(e <- csInv){
      var idx = csInv.indexOf(e)
      sum = 0
      for( i <- 0 to number-1){
        if(idx - i >= 0)
          sum += csInv(idx-i).close
      }
      values.append(sum/number)  
    }
    for(i <- 0 to number-2){
      values(i) = 0
    }
    
  }
  
  def PrintIndicator() = {
    for(e <- values){
      println(e.toString())
    }
  }
  
}