
object NetworkMap {
	//�binit���ɭԫإ�x*y��Neuron Map�A��tuple(x,y)�Ӱt��
  //Neuron����?
  var neuronMap:Map[Tuple2[Int,Int],Neuron] = Map()
  var converge:Boolean = false
  
  def init(){
    //�إ�Map�A���i����
    for (i <- 0 until SOM.mapX){
      for (j <- 0 until SOM.mapY){
        neuronMap+=((i,j)->new Neuron(i,j))
      }
    }
  }
  //�M�� ����learn()����execute()
  //�ݭn��compete()��X����{�׳̰���Neuron!
  //�ݭn��networkMutate()�Ӱ�Neuron�P�F��Neuron��weight�ȧ���
  def testWeight(){
	//�L�X�H�����ͪ�weight�����G�A���ݭn����
    neuronMap.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.test())
  }
  
  //���o���Ƴ̤j��Neuron
  def compete(id:String, input:Array[Double]):Neuron = {
    //���C��Neuron�p�����

	  //�C��Neuron�ӧO�p����ơA�����ۼv�T�A������B��!
	  neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.distanceCalculator(input))
	  var winner:Neuron = neuronMap.head._2
	  var neurons = neuronMap.values.toArray
	  //��j�p�A���i����
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
    //�@�H�o�D�����ɤѡA�o�̭��B�z���O�C��Neuron�U�O��o����input�������Ұ���weight�վ�A�i�H����!
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
	  //���C��Neuron�ӧO�M�ŦU�۪�����list�P�ǲߦ��ơA�i�H����
	  neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.refresh)
  }
  
  def printResult(){
	  //�C�L���G�A���i����
	  for (i <- 0 until SOM.mapX){
	    for (j <- 0 until SOM.mapY){
	      neuronMap((i,j)).printMember()
	    }
	  }
  }
  
  def exportResult(){
    //�N���G�̾�path/�ǲߥN��/Neuron/�������G.txt���覡�s��
    //�UNeuron�i�W�߿�X�ɮ�,�i����B��
    neuronMap.par.foreach((p:(Tuple2[Int,Int],Neuron)) => p._2.exportFile)
  }
  
}