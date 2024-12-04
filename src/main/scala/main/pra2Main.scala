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

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

//TODO: HIGHLIGHT: IMPORTANT PETA QUAN FAIG SERVIR VIQUI_FILES, EN LLOC DEL SUBSET, PETA EN EL VIQUI_PARSER CREC.

//TODO: object.wrapper potser peta perquè ès object. (object.wrapper d'intre object.wrapper? peta?)
//TODO: (0.2, List(Element)) as (String,List())????????
//TODO: crec que el mapper no importa per a l'efficiencia si hi passes a 1 o 2, però el reducer sí.

object pra2 extends App {
  val d: Double = 0.85;
  val viquiFilesPath = "test_viqui3/";

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

  val (viqui,nFilessss) = getListOfFiles2(viquiFilesPath);
  println(nFilessss)
  println(viqui);


  def readFilesRefs(path: String): Map[(String,List[String]),List[Double]] = {
    val (filesPath, nFiles) = getListOfFiles2(path);
    def mapperReadFiles(file: String, nothing: List[Nothing]): List[((String,List[String]),List[Nothing])] = { //List[Nothing] a List[nFiles]
      List(
        ViquipediaParse.parseViquipediaFile(file) match {
        case ResultViquipediaParsing(titol, _, refs) => ((titol,refs),List())
        case _ => throw new Exception("Error en el readFilesRefs.")
      })
    }

    def reducerReadFiles(titol: (String,List[String]), nothing: List[List[Nothing]]): ((String,List[String]),List[Double]) = { //només hi ha un double per (titol,refs)
      (titol,List(1.0/nFiles))
    }

    MRWrapper.execute(filesPath, mapperReadFiles, reducerReadFiles,1,1);
  }

  val viquiContingut = readFilesRefs(viquiFilesPath);
  println("viqui " + viquiContingut)

  /*
                input:List[(K1,List[V1])],
  mapping:(K1,List[V1]) => List[(K2,V2)],
  reducing:(K2,List[V2])=> (K2,V3),
   */


  //Map[(titol,refs),Pr)
  def PR(viquiCont: Map[(String,List[String]),List[Double]]): Unit = {
    val viquiContList = viquiCont.toList
    //titol és unic, per tant pr també és unic.
    //pr és un double unic, una llista amb un element.
    def mapperEnviarRef(titolRefs: (String,List[String]), pr: List[Double]): List[(String,Double)] = {
      val totalPr = pr.sum
      val nRefs = titolRefs._2.length
      //per totes les referencies de titol, envia als seus links el seu valor/nLinks.
      titolRefs._2.map(ref => (ref,totalPr/nRefs))
    }

    //titol és únic, per tant es pot agafar el refsPR.head._1, ja que totes les referencies
    def reducerRebreFromRef(titol: String, pr: List[Double]): (String,Double) = {
      //per a cada referencia, agrupa el valor.
      (titol, pr.sum)
    }

    //reducer reb i suma les pagines que l'apunten, mapper envia les dades.
    val valorsRefs = MRWrapper.execute(viquiContList,mapperEnviarRef, reducerRebreFromRef);
    println("valRefs: " + valorsRefs)
    val newRefMap: Map[(String,List[String]),List[Double]] = viquiCont.map {
      case ((key1,listKey),_) => ((key1,listKey),List(valorsRefs.getOrElse(key1, 0.0)))
      case _ => throw new Exception("Error en el PR, quan es passa de map a newRefMap.")
    }
    println("newRefMap: " + newRefMap)
  }

  PR(viquiContingut);




















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


  object Menu {

    def mainMenu(): Unit = {
      println()
      println("Main Menu:")
      println("1. Calculate the average number of references that all pages have.")
      println("2. Query.")
      println("3. Tf_idf.")
      println("4. Time measurement.")
      println("5. Exit")

      val choice = readLine("Please select an option (1-5): ").trim

      choice match {
        case "1" => option1()
        case "2" => option2()
        case "3" => option3()
        case "4" => option4()
        case "5" => println("Exiting... Goodbye!")
        case _ =>
          println("Invalid choice, please try again.")
          mainMenu()
      }
    }

    def option1(): Unit = {
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
          option1()
      }
    }

    def option2(): Unit = {
      println()
      println("Option 2. ")
      println("1. Do something with Option 2")
      println("2. Go Back to Main Menu")

      val choice = readLine("Please select an option (1-2): ").trim

      choice match {
        case "1" =>
          println("Doing something with Option 2...")
          option2()
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          option2()
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
  }
}