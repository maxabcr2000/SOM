
object NetworkMap {
	//在init的時候建立x*y的Neuron Map，用tuple(x,y)來配對
  //Neuron物件?
  var neuronMap:Map[Tuple2[Int,Int],Neuron] = Map()
  var converge:Boolean = false
  
  def init(){
    //建立Map，不可平行
    for (i <- 0 until SOM.mapX){
      for (j <- 0 until SOM.mapY){
        neuronMap+=((i,j)->new Neuron(i,j))
      }
    }
  }
  //然後 執行learn()接著execute()
  //需要有compete()找出接近程度最高的Neuron!
  //需要有networkMutate()來做Neuron與鄰近Neuron的weight值改變
  def testWeight(){
	//印出隨機產生的weight的結果，不需要平行
    neuronMap.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.test())
  }
  
  //取得分數最大的Neuron
  def compete(id:String, input:Array[Double]):Neuron = {
    //讓每個Neuron計算分數

	  //每個Neuron個別計算分數，不互相影響，做平行運算!
	  neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.distanceCalculator(input))
	  var winner:Neuron = neuronMap.head._2
	  var neurons = neuronMap.values.toArray
	  //比大小，不可平行
	  for (i <- 0 until neurons.length){
	     if (SOM.distanceMethod == 2 && 
	        winner.score< neurons(i).score) winner = neurons(i)
	     else if (SOM.distanceMethod == 1 && 
	        winner.score> neurons(i).score) winner = neurons(i)
	  }
	  winner.assign(id, input)
	//println("winner:" + winner.coordinates)
    winner
  }
  
  def mutate(winner:Neuron,input:Array[Double]){
    var nconverge = true
    val winnerX = winner.coordinates._1
    val winnerY = winner.coordinates._2
    //一人得道雞犬升天，這裡面處理的是每個Neuron各別對這次的input的反應所做的weight調整，可以平行!
    neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => {
      val neuronX = p._1._1
      val neuronY = p._1._2
      val distance = Math.sqrt(Math.pow(winnerX-neuronX,2) + Math.pow(winnerY-neuronY,2))
      nconverge = nconverge && p._2.weightChanging(input, distance)
    })
    converge = nconverge
  }
  
  def learn(id:String, input:Array[Double]){
	  var winner = compete(id, input)
	  mutate(winner,input)
  }
  
  def execute(id:String, input:Array[Double]){
	  var winner = compete(id, input)
  }
  
  def refresh(){
	  //讓每個Neuron個別清空各自的紀錄list與學習次數，可以平行
	  neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.refresh)
  }
  
  def printResult(){
	  //列印結果，不可平行
	  for (i <- 0 until SOM.mapX){
	    for (j <- 0 until SOM.mapY){
	      neuronMap((i,j)).printMember()
	    }
	  }
  }
  
  def exportResult(){
    //將結果依據path/學習代數/Neuron/分類結果.txt的方式存放
    //各Neuron可獨立輸出檔案,可平行運算
    neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.exportFile)
  }
  
}