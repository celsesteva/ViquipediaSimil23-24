import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import mr.ViquipediaParse._

import scala.io.StdIn.readLine
import java.io.File
import udg.objects.Viqui
import mr._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import mapreduce.ProcessListStrings

import scala.collection.immutable.Range
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

import ProcessListStrings._

//TODO: HIGHLIGHT: IMPORTANT PETA QUAN FAIG SERVIR VIQUI_FILES, EN LLOC DEL SUBSET, PETA EN EL VIQUI_PARSER CREC.

//TODO: object.wrapper potser peta perquè ès object. (object.wrapper d'intre object.wrapper? peta?)
//TODO: (0.2, List(Element)) as (String,List())????????
//TODO: crec que el mapper no importa per a l'efficiencia si hi passes a 1 o 2, però el reducer sí.

object pra2 extends App {
  val startTime = System.nanoTime();
  val d: Double = 0.85;
  val prSteps = 10;
  val epsilon = 1E-5;
  //TODO: es penja amb "test_viqui/" pq? List(); dict.size és 0.
  val viquiFilesPath = "viqui_files/"; //TODO: FER QUE PUGUIS CANVIAR AIXÒ I ELS STOPwORDS.
  println((System.nanoTime()-startTime)/1000000)

  def getListOfFiles(path: String): (List[(Double,List[String])],Int) = {
    val files = new File(path);
    val filesList = files.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).map(f => f.getPath.mkString)
    val nFiles = filesList.size
    val initialValue = 1.0/nFiles
    val result = for(file <- filesList) yield (initialValue,List(file))
    (result.toList, nFiles)
  }

  def getListOfFiles2(path:String): (List[(String,List[Nothing])],Int )= {
    val files = new File(path);
    val filesList = files.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).map(f => f.getPath.mkString)
    val nFiles = filesList.length
    val result = for(file <- filesList) yield (file,List())
    (result.toList, nFiles)
  }

  /*
                input:List[(K1,List[V1])],
  mapping:(K1,List[V1]) => List[(K2,V2)],
  reducing:(K2,List[V2])=> (K2,V3),
   */


  def inverseDocFreq(): Unit = {
    //idf = log |C| / df on |C| ´es el nombre de documents de C i df ´es el nombre de documents on
    //apareix el terme t.

    //Ara ja podem definir tf idf d’un terme t a un document d dins una col·lecci´o de documents
    //C com a: tf idf = tf × idf on tf ´es el nombre de vegades que apareix t al document d i idf
    //´es l’inverse document frequency de t per la col·lecci´o de documents C.
  }

  def PR(contingutOriginal: Map[(String,List[String]),List[Double]],nFiles: Int):  Map[(String, List[String]), List[Double]] = {
    //titol és unic, per tant pr també és unic. pr és un double unic, una llista amb un element.
    def mapperEnviarRef(titolRefs: (String,List[String]), pr: List[Double]): List[(String,Double)] = { //TODO: titolRefs (String, List[String]) -> (String, Array[string] for O(1) length.
      val totalPr = pr.sum
      val nRefs = titolRefs._2.length
      val prPerRef = if (nRefs > 0) totalPr / nRefs else 0.0
      titolRefs._2.map(ref => (ref,prPerRef))
    }
    //titol és únic, per tant es pot agafar el refsPR.head._1, ja que conte tots els prs que van a ell
    def reducerRebreFromRef(titol: String, pr: List[Double]): (String,Double) = {
      (titol, pr.sum)
    }

    //reducer reb i suma les pagines que l'apunten, mapper envia les dades.
    //TODO: mapreduce???? (en el reduce del mapReduce fer que el reducing sigui mapIndex, i el
    var steps = 0;
    var error: Double = 1;
    var newRefMap = contingutOriginal;
    while (steps < prSteps && error > epsilon) {
      val valorsRefs = MRWrapper.execute(newRefMap.toList, mapperEnviarRef, reducerRebreFromRef,32,32);
      //println("valRefs: " + valorsRefs) //els que no reben valors de cap, no estan aquí. però també hi ha links a pagines externes.

      // Calculate the new PageRank with damping factor //TODO: FER MapReduce.
      val updatedRefMap = contingutOriginal.map {
        case ((key1, listKey), _) => ((key1, listKey), List(valorsRefs.getOrElse(key1, 0.0)))
          val incomingPR = valorsRefs.getOrElse(key1, 0.0)
          val newPR = ((1 - d) / nFiles) + d * incomingPR
          ((key1, listKey), List(newPR))
        case _ => throw new Exception("Error en el PR, quan es passa de map a newRefMap.")
      }

      error = newRefMap.zip(updatedRefMap).map {
        case (((_, _), oldPR), ((_, _), newPR)) =>
          math.abs(oldPR.head - newPR.head)
      }.max
      newRefMap = updatedRefMap // Update for the next iteration
      //println("newRefMap: " + newRefMap)
      steps += 1;
    }
    newRefMap
  }

  println((System.nanoTime()-startTime)/1000000)

  Menu.mainMenu()

  def timeMeasurement[A](function: => A): (Double,A) = {
    val before = System.nanoTime
    val result = function
    ((System.nanoTime - before)/1000000.0, result)
  }


  def averageLinks(): Unit = {
    val (viquiFiles,nFiles) = getListOfFiles(viquiFilesPath); //TODO: el (0.2 List(String)) a (string, List())
    def mappingReadFiles(initialValue:Double, files:List[String]): List[(Int,Int)]  = {
      files.map(f => ViquipediaParse.parseViquipediaFile(f) match {
        case ResultViquipediaParsing(_,_,refs) => (1,refs.length)
      })
    }
    def reducingReadFiles(ones:Any, resta: List[Int]): (String,Double) = { //it forces ones to be :Any pq no l'utilitzo.
      ("Mitjana",resta.sum/nFiles.toDouble)
    }
    val resultatsMap = MRWrapper.execute(viquiFiles,mappingReadFiles,reducingReadFiles,32,32)
    val res = resultatsMap.headOption; //només agafo el primer pq retorna només un resultat.
    res match {
      case Some(result) => println("Average links:  " + result._2)
      case _ => System.err.println("Error in the calculation")
    }
  }

  def calcPageRankBasedOnQuery(query: String): Unit = {
    val start = System.nanoTime()
    ////TODO: map reduce per a filtrar els fitxers que tenen la paraula search/paraules
    val filename ="stopwordscatalanet.txt"; //todo: CHANGE THIS LOL TO A general one.
    val stopWords = ProcessListStrings.llegirFitxer(filename);
    val stopWordsSet = Viqui.normalize(stopWords).toSet
    val myQuery = Viqui.normalize(query).filterNot(word => stopWordsSet.contains(word))//.mkString(" ");
    val myQuerySize = myQuery.size;
    //val files = ProcessListStrings.getListOfFiles(viquiFilesPath);
    def readFilesPageRank(path: String): (Map[(String,List[String]),List[Double]],Int) = { //TODO: Map[(String,List[String]) -> Map[(String,Array[String])
      val (filesPath, nFiles) = getListOfFiles2(path);
      def mapperReadFiles(file: String, nothing: List[Nothing]): List[((String,List[String]),List[Nothing])] = { //List[Nothing] a List[nFiles]
        List(
          ViquipediaParse.parseViquipediaFile(file) match {
            case ResultViquipediaParsing(titol, cont, refs) =>
              val nonStopCont = Viqui.normalize(cont).filterNot(str => stopWordsSet.contains(str));
              val ngram = Viqui.ngrames(nonStopCont,myQuerySize);
              if(ngram.contains(myQuery)){
                ((titol,refs),List())
              }
              else{
                (("",List[String]()),List())
              }
            case _ => throw new Exception("Error en el readFiles.")
          })
      }
      def reducerReadFiles(titol: (String,List[String]), nothing: List[List[Nothing]]): ((String,List[String]),List[Double]) = { //només hi ha un double per (titol,refs)
        (titol,List(1.0/nFiles))
      }
      (MRWrapper.execute(filesPath, mapperReadFiles, reducerReadFiles,32,32),nFiles)
    }
    val (fileCont,nFiles) = readFilesPageRank(viquiFilesPath);
    val keyToRemove = ("", List[String]()) // Adjust according to the actual key type
    val newFileCont = fileCont - keyToRemove
    val pg = PR(newFileCont,nFiles)
    pg.foreach( f => println(f._1._1 + " " + f._2.sum))
    println("PGTime " + (System.nanoTime()-start)/1000000 + "ms");
  }


  object Menu {

    def mainMenu(): Unit = {
      println()
      println("Main Menu:")
      println("1. Calculate the average number of references that all pages have.")
      println("2. Recommendation based on query.")
      println("3. Tf_idf.")
      println("4. Time measurement.")
      println("5. Change file folder.")
      println("5. Exit.")

      val choice = readLine("Please select an option (1-5): ").trim

      choice match {
        case "1" => optionAverageRefs()
        case "2" => optionRecomendation()
        case "3" => option3()
        case "4" => option4()
        case "5" => println("Exiting... Goodbye!")
        case _ =>
          println("Invalid choice, please try again.")
          mainMenu()
      }
    }

    def optionAverageRefs(): Unit = {
      println()
      println("Option 1. Calculate the average number of references that all pages have.")
      println("1. Calculate.")
      println("2. Go Back to Main Menu")

      val choice = readLine("Please select an option (1-2): ").trim

      choice match {
        case "1" =>
          averageLinks();
          mainMenu()
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          optionAverageRefs()
      }
    }

    def optionRecomendation(): Unit = {
      def optionAskQuery(): Unit = {
        println()
        val query = askQuery();
        println(s"1. Calculate PR based on your query '$query'.")
        println("2. Change query.")
        println("3. Go back.")
        def askQueryChoice() {
          val choice = readLine("Please select an option (1-3): ").trim
          choice match {
            case "1" =>
              calcPageRankBasedOnQuery(query);
              mainMenu();
            case "2" =>
              optionAskQuery()
            case "3" =>
              optionRecomendation();
            case _ =>
              println();
              println("Invalid choice, please try again.")
              println(s"1. Calculate PR based on your query '$query'.")
              println("2. Change query.")
              println("3. Go back.")
              askQueryChoice()
          }
        }
        askQueryChoice()
      }

      println()
      println("Option 2: Recommendation based on query.")
      println("1. Enter the query.")
      println("2. Go Back to Main Menu.")

      val choice = readLine("Please select an option (1-2): ").trim
      choice match {
        case "1" =>
          optionAskQuery();
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          optionRecomendation()
      }
    }

    def option3(): Unit = {
      println()
      println("Option 3.")
      println("1. Do something with Option 3")
      println("2. Go Back to Main Menu")

      val choice = readLine("Please select an option (1-2): ").trim

      choice match {
        case "1" =>
          println("Doing something with Option 3...")
          option3()
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          option3()
      }
    }

    def option4(): Unit = {
      println()
      println("Option 4: Time measurement.")
      println("1. Do something with Option 4")
      println("2. Go Back to Main Menu")

      val choice = readLine("Please select an option (1-2): ").trim

      choice match {
        case "1" =>
          println("Doing something with Option 3...")
          option4()
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          option4()
      }
    }

    def askQuery(): String = {
      print("Query: ")
      val query = readLine().trim;
      println("Your query is \'" + query + "\'.");
      query;
    }
  }
}