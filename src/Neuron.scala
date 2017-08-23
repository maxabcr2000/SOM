import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

class Neuron(i:Int, j:Int) {
  //在建構的時候，隨機產生對應Dimension維度的weight
  //neuronMutate()依照公式改變Neuron的weight值
	val coordinates = (i,j)
	var member:List[Tuple2[String, Array[Double]]] = List() //List of (id:String, input:Array[Double])
	var score:Double = 0;
	var weightArray:Array[Double] = Array.tabulate(SOM.dim){i => SOM.wLowerBound + (SOM.wUpperBound-SOM.wLowerBound)*scala.util.Random.nextDouble()}
	
	var learnGain:Double = SOM.learnGain
	var distanceCalculator:(Array[Double])=>Double = {
		def euclideanDis(b:Array[Double]):Double={
			var sum:Double=0
			//做的是加總的動作，不可平行
			for (i<-0 until weightArray.length){
			  sum = sum + Math.pow(weightArray(i)-b(i), 2)
			}
			score = Math.sqrt(sum)
			score
		}
		def sphericalArcDis(b:Array[Double]):Double={
			var sum:Double=0
			//做的是加總的動作，不可平行
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
	  //列印結果，不可平行
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
	  //列印結果，不可平行
	  member.foreach((m:Tuple2[String,Array[Double]])=>print(m._1 + " "))
	  println()
	}
	
	def weightChanging(b:Array[Double], neiborDis:Double):Boolean={
	  var converge = false
	  val maxGeneration:Int = {
	    if (SOM.maxGeneration<=0) 1000
	    else SOM.maxGeneration
	  }
	  //每個weight的dimension各自去做，但都需要對converge做改變，所以不可平行
	  for (i <- 0 until weightArray.length){
	    val deltaW = learnGain * 
	    		(1- SOM.learnTimes/maxGeneration) *
	    		(1 - neiborDis/Math.sqrt(Math.pow(SOM.mapX,2) + Math.pow(SOM.mapY,2))) * 
	    		(b(i)-weightArray(i))
	    weightArray(i) = weightArray(i) + deltaW
	    //除非全部的象限之weight都收斂，否則converge不為true
	    //離Winner的距離夠近的neuron才納入判斷收斂的條件，因為太遠的Neuron的weight改變量太小，如果也納入考量會很難達到收斂條件
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
	  if (coordinates==(SOM.BBPNTestX,SOM.BBPNTestY)) expectedTarget = 1	//代表這個Neuron的分類是使用者想要過濾的分類，所以讓預期結果為1
	  
	  val filePath = "LearnGeneration_" + SOM.learnTimes + "/Neuron" + coordinates + ".txt";
	  var fw = new FileWriter(filePath, true);
	  var bw = new BufferedWriter(fw);
	  
	  //串接要輸出的結果，不可平行
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