import java.io.File;

object SOM {
  var mapX = 0
  var mapY = 0
  var dim = 0
  var wUpperBound = 0.0
  var wLowerBound = 0.0
  var distanceMethod = 1
  //讓使用者決定要使用哪一種方法!
  //看是要使用歐基里德距離還是球弧距離
  var maxGeneration = 0
  var learnGain = 0.0
  var learnTimes = 0
  var tolerate = 0.0
  var learningData:List[Array[String]] = List[Array[String]]()
  var executingData:List[Array[String]] = List[Array[String]]()
  
  //for testing
  var BBPNTestX = 0
  var BBPNTestY = 0
    def main(args: Array[String]) {
      mapX = readLine("type in x: ").toInt
      mapY = readLine("type in y: ").toInt
      
      wUpperBound = readLine("type in Neural Weight Upperbound: ").toDouble
      wLowerBound = readLine("type in Neural Weight Lowerbound: ").toDouble
      distanceMethod = readLine("choose the distance method.\ntype in 1 for Educliden Distance, 2 for Spherical Arc Distance.").toInt
      //讓使用者決定要使用哪一種方法!
      //看是要使用歐基里德距離還是球弧距離
      maxGeneration = readLine("type in Max Generation:").toInt
      tolerate = readLine("type in tolerate t, the converge condition will be all the neibor's deltaW/weight smaller than t: ").toDouble
      learnGain = readLine("type in Learn Gain: ").toDouble
      BBPNTestX = readLine("請輸入想過濾的分群代碼x: ").toInt
      BBPNTestY = readLine("請輸入想過濾的分群代碼y: ").toInt      
      //讀取學習用的txt資料
      for(line <- scala.io.Source.fromFile("Learn_Data.txt").getLines()){
    	  //learningData = line.split("\t")::learningData
          learningData = line.split(" ")::learningData
      }
      
      dim = learningData.head.length-1
      //println("learningData.length():" + learningData.length)
      
      NetworkMap.init()
      //NetworkMap.testWeight()
      //先呼叫NetworkMap的init(mapX,mapY)，先建立好Neuron物件
      
      //平行?或者逐筆執行learn的過程?
      //print出learning完的分類結果
      learnTimes = 1 
      while({if (maxGeneration<=0) true
      else learnTimes<=maxGeneration
      } )
      {
        println("-------------------------Learning Generation " + learnTimes + "-------------------------")
    	
        var id = ""
        var input = Array[Double]()
        learningData.foreach((data:Array[String])=> {
            //以下為800app版本,id為最後一筆資料
	        id = data.last
		    input = data.init.map(_.toDouble)    	  
    		NetworkMap.learn(id, input)
    	})
      	NetworkMap.printResult()
      	var dirBool = new File("LearnGeneration_" + learnTimes).mkdir();
      	NetworkMap.exportResult()
      	//清空Neuron的分類
      	NetworkMap.refresh()
      	learnTimes = learnTimes + 1
      }

      //接著讓使用者輸入executing的資料，每次輸入就print出分類結果
      for(line <- scala.io.Source.fromFile("Execute_Data.txt").getLines()){
    	  //executingData = line.split("\t")::executingData
          executingData = line.split(" ")::executingData
      }
      
      println("-------------------------Executing-------------------------")
      executingData.foreach((data:Array[String])=> {
        //以下為800app版本,id為最後一筆資料
	    var id = data.last
		var input = data.init.map(_.toDouble)  
        NetworkMap.execute(id, input)
      })
      NetworkMap.printResult()
    }
}