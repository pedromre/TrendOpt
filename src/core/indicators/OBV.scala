package core.indicators

import core.CandleSeries

class OBV(var cs:CandleSeries) {
  var values = scala.collection.mutable.ListBuffer.empty[Float]
  var candlesL = cs.CS
  
  def UpdateIndicator(){
    var temp:Float = 0
    var obv:Long = 0
    for(e <- candlesL){
      var change = e.close - temp
    
      if(change < 0){
        obv -= e.volume  
      }else if(change > 0){
        obv += e.volume
      }else{
        obv = obv
      }
      values.append(obv)
      temp = e.close
    }
    values(0) = 0
  }
  
  def PrintIndicator() = {
    for(e <- values){
      println(e.toString())
    }
  }
  
}