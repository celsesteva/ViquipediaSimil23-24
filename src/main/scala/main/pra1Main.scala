import udg.objects.Simil

import java.io.File
import scala.io.Source
/*
 foldLeft, contains, updated, toList,
map, isLetter, toLower, split, filter, sortWith, take, drop, toFloat, length,
filterNot, sliding, groupBy, zip, ...
*/

object Main extends App {
  def getListOfFiles(path: String): List[String] = {
    val files = new File(path);
    files.listFiles.filter(f => f.isFile && f.getName.endsWith(".txt")).map(f => f.getPath.mkString).toList;
  }
  def getListOfFilesNames(path: String): List[String] = {
    val files = new File(path);
    files.listFiles.filter(f => f.isFile && f.getName.endsWith(".txt")).map(f => f.getName.mkString).toList;
  }

  //exmeples de la pràctica que es demanen.
  val fileAliceInWonderland = "primeraPartPractica/pg11.txt"
  val fileAliceInWonderlandContents = Source.fromFile(fileAliceInWonderland).mkString
  //1: Freqüències de paraules:
  Simil.printResultsFreq(fileAliceInWonderlandContents)
  val fileStopWords = "primeraPartPractica/english-stop.txt"
  val fileStopWordsContents = Source.fromFile(fileStopWords).mkString;
  //2: Sense stop-words:
  Simil.printResultsNonStopWords(fileAliceInWonderlandContents,Simil.normalize(fileStopWordsContents))
  //3: Distribució de paraules:
  Simil.paraulafreqfreq(fileAliceInWonderlandContents);
  //4.1: ngrames:
  Simil.printNgram(Simil.ngrames(fileAliceInWonderlandContents,3).sortWith(_._2>_._2),10)
  //4.2: ngrames sense stop-words:
  Simil.printNgram(Simil.ngramesNonstopwords(fileAliceInWonderlandContents,3,Simil.normalize(fileStopWordsContents).toSet).sortWith(_._2>_._2),10)
  val fileAliceInWonderland2 = "primeraPartPractica/pg12.txt"
  val fileAliceInWonderlandContents2 = Source.fromFile(fileAliceInWonderland2).mkString
  //5: Vector space model:
  println(Simil.cosinesim(fileAliceInWonderlandContents,fileAliceInWonderlandContents2,Simil.normalize(fileStopWordsContents),1))
  println();
  //tots els exemples de la pràctcia fets.


  //agafo els paths de les carpetes.
  val filesPath = "primeraPartPractica/";
  val filesList = getListOfFiles(filesPath);
  val stopFiles: List[String] = filesList.filter(_.contains("stop"));
  val files: List[String] = filesList.filterNot(_.contains("stop"));
  val filesNamesAll: List[String] = getListOfFilesNames(filesPath);
  val stopNames: List[String] = filesNamesAll.filter(_.contains("stop"));
  val fileNames: List[String] = filesNamesAll.filterNot(_.contains("stop"));

  //obting els stopWords.
  val stopWords = Simil.normalize(Source.fromFile(stopFiles.head).mkString); //I will just do it with english-stop words.

  //funció per a calcular tots els cosinesims.
  def calculateAllCosinesim(files: List[String],stopWords: List[String], n: Int): List[Float] = {
    val result = for (first <- files; second <- files) yield {
      val x = Source.fromFile(first).mkString;
      val y = Source.fromFile(second).mkString;
      Simil.cosinesim(x,y,stopWords,n);
    }
    result;
  }

  //fa el print de cuadricula.
  def printResults(fileNames: List[String], values: List[List[Float]]): Unit = (fileNames, values) match {
    case (Nil, _) => println("Empty file names")
    case (head :: tail, Nil) => println("Empty values list")
    case (fileNames, head :: tail) =>
      // Fa print dels noms de les columnes.
      println(f"${" " * 20} | " + fileNames.map(f => f"$f%-20s").mkString(" | ")) //fa print de fileNames, però abans el tranforma a l'output adequat.
      println("-" * (fileNames.size * 24 + 5)) // Just to make the separator line

      // Fa el print de les files amb el nom de la fila.
      fileNames.zip(values).foreach { case (rowName, similarities) =>
        println(f"$rowName%-20s | " +
          similarities.map(sim => f"$sim%-20.4f").mkString(" | "))
      }
  }


  //ara ho faig 5 cops per a trobar els resultats.
  var before = System.nanoTime;
  var n: Int = 1;
  var result = calculateAllCosinesim(files,stopWords,n);
  var totalTime=System.nanoTime-before

  println("Time " + totalTime / 1000000 + "ms")
  println("Using " + stopNames.head + s" and a window of $n:")
  var listOfLists: List[List[Float]] = result.grouped(files.size).toList;
  printResults(fileNames,listOfLists);
  println()
  println()



  before = System.nanoTime;
  n = n+1;
  result = calculateAllCosinesim(files,stopWords,n);
  totalTime=System.nanoTime-before

  println("Time " + totalTime / 1000000 + "ms")
  println("Using " + stopNames.head + s" and a window of $n:")
  listOfLists = result.grouped(files.size).toList;
  printResults(fileNames,listOfLists);
  println()
  println()




  before = System.nanoTime;
  n = n+1;
  result = calculateAllCosinesim(files,stopWords,n);
  totalTime=System.nanoTime-before

  println("Time " + totalTime / 1000000 + "ms")
  println("Using " + stopNames.head + s" and a window of $n:")
  listOfLists = result.grouped(files.size).toList;
  printResults(fileNames,listOfLists);
  println()
  println()



  before = System.nanoTime;
  n = n+1;
  result = calculateAllCosinesim(files,stopWords,n);
  totalTime=System.nanoTime-before

  println("Time " + totalTime / 1000000 + "ms")
  println("Using " + stopNames.head + s" and a window of $n:")
  listOfLists = result.grouped(files.size).toList;
  printResults(fileNames,listOfLists);
  println()
  println()



  before = System.nanoTime;
  n = n+1;
  result = calculateAllCosinesim(files,stopWords,n);
  totalTime=System.nanoTime-before

  println("Time " + totalTime / 1000000 + "ms")
  println("Using " + stopNames.head + s" and a window of $n:")
  listOfLists = result.grouped(files.size).toList;
  printResults(fileNames,listOfLists);
  println()
  println()
}