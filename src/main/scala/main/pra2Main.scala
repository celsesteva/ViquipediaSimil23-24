import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import mr.ViquipediaParse._

import java.io.File
import udg.objects.Viqui

import mr._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

object pra2 extends App {
  def timeMeasurement[A](function: => A): (Double,A) = {
    val before = System.nanoTime
    val result = function
    ((System.nanoTime - before)/1000000.0, result)
  }

  def getListOfFiles(path: String): (List[(Double,List[String])],Int) = {
    val files = new File(path);
    val filesList = files.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).map(f => f.getPath.mkString)
    val nFiles = filesList.size
    val initialValue = 1.0/nFiles
    val result = for(file <- filesList) yield (initialValue,List(file))
    (result.toList, nFiles)
  }

  println("Start")
  val viquiFilesPath = "small_subset_viqui_files/";
  val (viquiFiles,nFiles) = getListOfFiles(viquiFilesPath);
  val (totalTime,result) = timeMeasurement(ViquipediaParse.parseViquipediaFile(viquiFiles.head._2.head));
  println(viquiFiles)
  println(s"Time taken: " + totalTime + "ms")



  def mappingReadFiles(initialValue:Double, files:List[String]): List[(Int,Int)]  = {
    files.map(f => ViquipediaParse.parseViquipediaFile(f) match {
      case ResultViquipediaParsing(_,_,refs) => println(refs.length); (1,refs.length)
    })
  }

  def reducingReadFiles(ones:Any, resta: List[Int]): (String,Double) = { //it forces ones to be :Any pq no l'utilitzo.
    ("Mitjana",resta.sum/nFiles.toDouble)
  }


  val resultats = MRWrapper.execute(viquiFiles,mappingReadFiles,reducingReadFiles,32,32)

  println(resultats)



  /*
    val systema: ActorSystem = ActorSystem("sistemaPR")
    def mappingPR(titol:String, refs:Set[String]) :List[(String, Int)] =
      for (word <- words) yield (word, 1)

    def reducingPR(word:String, nums:List[Int]):(String,Int) =
      (word, nums.sum)

    val pageRank = systema.actorOf(Props(new MR((result.titol,result.refs),mappingPR,reducingPR)), name = "masterPageRank")
    implicit val timeout = Timeout(10000 seconds)
    var futurePageRank = pageRank ? mr.MapReduceCompute()
    println("Awaiting")
    val pageRankResult:Map[String,Int] = Await.result(futurePageRank,Duration.Inf).asInstanceOf[Map[String,Int]]
    println("Results Obtained")
    for(v<-pageRankResult) println(v)
    // Fem el shutdown del actor system
    println("shutdown")
    //systema.terminate()
    println("ended shutdown")
    // com tancar el sistema d'actors.

   */
}