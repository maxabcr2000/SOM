import java.io.File;

object SOM {
  var mapX = 0
  var mapY = 0
  var dim = 0
  var wUpperBound = 0.0
  var wLowerBound = 0.0
  var distanceMethod = 1
  //���ϥΪ̨M�w�n�ϥέ��@�ؤ�k!
  //�ݬO�n�ϥμڰ򨽼w�Z���٬O�y���Z��
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
      //���ϥΪ̨M�w�n�ϥέ��@�ؤ�k!
      //�ݬO�n�ϥμڰ򨽼w�Z���٬O�y���Z��
      maxGeneration = readLine("type in Max Generation:").toInt
      tolerate = readLine("type in tolerate t, the converge condition will be all the neibor's deltaW/weight smaller than t: ").toDouble
      learnGain = readLine("type in Learn Gain: ").toDouble
      BBPNTestX = readLine("�п�J�Q�L�o�����s�N�Xx: ").toInt
      BBPNTestY = readLine("�п�J�Q�L�o�����s�N�Xy: ").toInt      
      //Ū���ǲߥΪ�txt���
      for(line <- scala.io.Source.fromFile("Learn_Data.txt").getLines()){
    	  //learningData = line.split("\t")::learningData
          learningData = line.split(" ")::learningData
      }
      
      dim = learningData.head.length-1
      //println("learningData.length():" + learningData.length)
      
      NetworkMap.init()
      //NetworkMap.testWeight()
      //���I�sNetworkMap��init(mapX,mapY)�A���إߦnNeuron����
      
      //����?�Ϊ̳v������learn���L�{?
      //print�Xlearning�����������G
      learnTimes = 1 
      while({if (maxGeneration<=0) true
      else learnTimes<=maxGeneration
      } )
      {
        println("-------------------------Learning Generation " + learnTimes + "-------------------------")
    	
        var id = ""
        var input = Array[Double]()
        learningData.foreach((data:Array[String])=> {
            //�H�U��800app����,id���̫�@�����
	        id = data.last
		    input = data.init.map(_.toDouble)    	  
    		NetworkMap.learn(id, input)
    	})
      	NetworkMap.printResult()
      	var dirBool = new File("LearnGeneration_" + learnTimes).mkdir();
      	NetworkMap.exportResult()
      	//�M��Neuron������
      	NetworkMap.refresh()
      	learnTimes = learnTimes + 1
      }

      //�������ϥΪ̿�Jexecuting����ơA�C����J�Nprint�X�������G
      for(line <- scala.io.Source.fromFile("Execute_Data.txt").getLines()){
    	  //executingData = line.split("\t")::executingData
          executingData = line.split(" ")::executingData
      }
      
      println("-------------------------Executing-------------------------")
      executingData.foreach((data:Array[String])=> {
        //�H�U��800app����,id���̫�@�����
	    var id = data.last
		var input = data.init.map(_.toDouble)  
        NetworkMap.execute(id, input)
      })
      NetworkMap.printResult()
    }
}