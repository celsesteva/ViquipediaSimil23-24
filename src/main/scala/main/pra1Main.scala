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


  //TODO: fer que vagi rapid. ////////////---------------- FET
  //TODO: fer que faci tots amb tots PER A totes les funcions.
  //TODO: fer document.
  //TODO: commentate.
  /*
  Lliurament
 Les pr`actiques es poden fer en equips de dos.
 Cal que documenteu el codi.
 Lliurareu un document amb el codi de cada apartat i exemples d’execuci´o aix´ı com les
taules de resultats de les comparacions finals.
 Properament es publicar`a la segona part de la pr`actica i les instruccions de lliurament.
   */

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
  //tots els exemples de la pràctcia fets.


  val filesPath = "primeraPartPractica/";

  val filesList = getListOfFiles(filesPath);

  val stopFiles: List[String] = filesList.filter(_.contains("stop"));
  println(stopFiles)
  val files: List[String] = filesList.filterNot(_.contains("stop"));
  println(files);

  val filePathPg11 = "primeraPartPractica/pg11.txt"
  val fileContentsPg11 = Source.fromFile(filePathPg11).mkString
  val filePathPg12 = "primeraPartPractica/pg12.txt"
  val fileContentsPg12 = Source.fromFile(filePathPg12).mkString
  val filePathPg74 = "primeraPartPractica/pg74.txt"
  val fileContentsPg74 = Source.fromFile(filePathPg74).mkString
  val filePathPg2500 = "primeraPartPractica/pg2500-net.txt"
  val fileContentsPg2500 = Source.fromFile(filePathPg2500).mkString


  val filePathStop = "primeraPartPractica/english-stop.txt"
  val fileContentsStop = Source.fromFile(filePathStop).mkString

  val stopWords = Simil.normalize(fileContentsStop);

  //println(Simil.nonstopfreq(fileContentsPg11,stopWords.distinct).sortWith(_._2 > _._2))

  //println(Simil.freq(fileContents))
  //Simil.paraulafreqfreq(fileContentsPg11);
  Simil.printWindow(Simil.ngrames(fileContentsPg11,3).sortWith(_._2 > _._2).take(10),10);
  //println(Simil.freq(fileContentsPg11))
  //println(Simil.nonstopfreq(fileContentsPg11,Simil.normalize(fileContentsStop)))
  println(Simil.cosinesim(fileContentsPg11,fileContentsPg12,stopWords,1));
}