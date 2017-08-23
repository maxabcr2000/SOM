import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

class Neuron(i:Int, j:Int) {
  //�b�غc���ɭԡA�H�����͹���Dimension���ת�weight
  //neuronMutate()�̷Ӥ�������Neuron��weight��
	val coordinates = (i,j)
	var member:List[Tuple2[String, Array[Double]]] = List() //List of (id:String, input:Array[Double])
	var score:Double = 0;
	var weightArray:Array[Double] = Array.tabulate(SOM.dim){i => SOM.wLowerBound + (SOM.wUpperBound-SOM.wLowerBound)*scala.util.Random.nextDouble()}
	
	var learnGain:Double = SOM.learnGain
	var distanceCalculator:(Array[Double])=>Double = {
		def euclideanDis(b:Array[Double]):Double={
			var sum:Double=0
			//�����O�[�`���ʧ@�A���i����
			for (i<-0 until weightArray.length){
			  sum = sum + Math.pow(weightArray(i)-b(i), 2)
			}
			score = Math.sqrt(sum)
			score
		}
		def sphericalArcDis(b:Array[Double]):Double={
			var sum:Double=0
			//�����O�[�`���ʧ@�A���i����
			for (i<-0 until weightArray.length){
			  sum = sum + weightArray(i)*b(i)
			}
			score = sum
			score
		}
		if (SOM.distanceMethod==1) euclideanDis
		else sphericalArcDis
	}
	def test(){
	  println("Neuron(" + i + "," + j + ")")
	  //�C�L���G�A���i����
	  for (i <- 0 until weightArray.length)
	  {
	    println(weightArray(i))
	  }
	}
	
	def assign(id:String, input:Array[Double]){
	  member = (id, input) :: member
	}
	
	def refresh(){
	  member = List()
	}
	
	def printMember(){
	  println(coordinates.toString + ":" + member.size)
	  //�C�L���G�A���i����
	  member.foreach((m:Tuple2[String,Array[Double]])=>print(m._1 + " "))
	  println()
	}
	
	def weightChanging(b:Array[Double], neiborDis:Double):Boolean={
	  var converge = false
	  val maxGeneration:Int = {
	    if (SOM.maxGeneration<=0) 1000
	    else SOM.maxGeneration
	  }
	  //�C��weight��dimension�U�ۥh���A�����ݭn��converge�����ܡA�ҥH���i����
	  for (i <- 0 until weightArray.length){
	    val deltaW = learnGain * 
	    		(1- SOM.learnTimes/maxGeneration) *
	    		(1 - neiborDis/Math.sqrt(Math.pow(SOM.mapX,2) + Math.pow(SOM.mapY,2))) * 
	    		(b(i)-weightArray(i))
	    weightArray(i) = weightArray(i) + deltaW
	    //���D�������H����weight�����ġA�_�hconverge����true
	    //��Winner���Z������neuron�~�ǤJ�P�_���Ī�����A�]���ӻ���Neuron��weight���ܶq�Ӥp�A�p�G�]�ǤJ�Ҷq�|�����F�즬�ı���
	    if (neiborDis<=Math.sqrt(2)){
	      //println("deltaW/weight: " + deltaW/weightArray(i) + " distance:" + neiborDis)
	      if (Math.abs(deltaW/weightArray(i)) < SOM.tolerate) {
	        converge = true
	      }
	     } 
	  }
	  converge
	}
	
	def exportFile(){
	  var expectedTarget = 0
	  if (coordinates==(SOM.BBPNTestX,SOM.BBPNTestY)) expectedTarget = 1	//�N��o��Neuron�������O�ϥΪ̷Q�n�L�o�������A�ҥH���w�����G��1
	  
	  val filePath = "LearnGeneration_" + SOM.learnTimes + "/Neuron" + coordinates + ".txt";
	  var fw = new FileWriter(filePath, true);
	  var bw = new BufferedWriter(fw);
	  
	  //�걵�n��X�����G�A���i����
	  member.foreach((m:Tuple2[String,Array[Double]])=>{
		  	var lineString = ""
		    for (i<-0 until m._2.length){
		      //println("elements:" + m._2(i))
		      lineString = lineString + m._2(i)
		      if (i < m._2.length-1) lineString = lineString + " "
		    }
		    lineString = lineString + "/" + expectedTarget + "\n"
		    //println("LineString:" + lineString)
		    bw.write(lineString)
	    })
	  
	  bw.close()
	  fw.close()
	}
}