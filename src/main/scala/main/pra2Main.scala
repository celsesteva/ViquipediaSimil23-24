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

import scala.util.matching.Regex

//TODO: HIGHLIGHT: IMPORTANT PETA QUAN FAIG SERVIR VIQUI_FILES, EN LLOC DEL SUBSET, PETA EN EL VIQUI_PARSER CREC.

//TODO: object.wrapper potser peta perquè ès object. (object.wrapper d'intre object.wrapper? peta?)
//TODO: (0.2, List(Element)) as (String,List())????????
//TODO: crec que el mapper no importa per a l'efficiencia si hi passes a 1 o 2, però el reducer sí.

object tests extends App {
  val ref = new Regex("\\[\\[[^\\]]*\\]\\]")
  //println("La pagina es: " + titol)
  //println("i el contingut: ")
  //println(contingut)
  val refs = List("[[]]")

  val disallowedChars = Set(':', '#')
  val disallowedPattern = "[:#]".r

  // elimino les que tenen :
  //val filteredRefs = refs.filterNot(ref => disallowedChars.exists(c => ref.contains(c)))
  val filteredRefs = refs.filterNot(ref => disallowedPattern.findFirstIn(ref).isDefined).filter(_.isEmpty)

  //elimino [[, | i ]]
  //TODO: PETA, SPLIT ESTA BUIT, FAIG MALAMENT, EM PETO COSES QUE POTSER NO TOQUEN, ETC....
  val cleanedRefs = filteredRefs.map(ref => ref.split("\\[\\[|\\]\\]|\\|")(1)).distinct; //removes repetits
}

object pra2 extends App {
  val startTime = System.nanoTime();
  val d: Double = 0.85;
  val prSteps = 10;
  val epsilon = 1E-5;
  var nMappers = 100; //make option to change.
  var nReducers = 100;
  //TODO: es penja amb "test_viqui/" pq? List(); dict.size és 0.
  //val viquiFilesPath = "viqui_files/"; //TODO: FER QUE PUGUIS CANVIAR AIXÒ I ELS STOPwORDS.
  val viquiFilesPath = "test_viqui3/";
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

  def PR(contingutOriginal: Map[(String,List[String]),List[Double]],nFiles: Int):  Map[(String, List[String]), List[Double]] = {
    //IF contingutOriginal és buit, peta.
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
      val valorsRefs = MRWrapper.execute(newRefMap.toList, mapperEnviarRef, reducerRebreFromRef,nMappers,nReducers);
      //println("valRefs: " + valorsRefs) //els que no reben valors de cap, no estan aquí. però també hi ha links a pagines externes.

      // Calculate the new PageRank with damping factor //TODO: FER MapReduce.
      val updatedRefMap = contingutOriginal.map {
        case ((key1, listKey), _) =>
          val incomingPR = valorsRefs.getOrElse(key1, 0.0)
          val newPR = ((1 - d) / nFiles) + d * incomingPR
          ((key1, listKey), List(newPR))
        case _ => throw new Exception("Error en el PR, quan es passa de map a newRefMap.")
      }

      error = newRefMap.zip(updatedRefMap).map {
        case (((_, _), oldPR), ((_, _), newPR)) =>
          math.abs(oldPR.head - newPR.head)
      }.sum
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
        case ResultViquipediaParsing(_,_,refs) => /*println(refs);*/(1,refs.length)
      })
    }
    def reducingReadFiles(ones:Any, resta: List[Int]): (String,Double) = { //it forces ones to be :Any pq no l'utilitzo.
      ("Mitjana",resta.sum/nFiles.toDouble)
    }
    val resultatsMap = MRWrapper.execute(viquiFiles,mappingReadFiles,reducingReadFiles,nMappers,nReducers)
    val res = resultatsMap.headOption; //només agafo el primer pq retorna només un resultat.
    res match {
      case Some(result) => println("Average links:  " + result._2)
      case _ => System.err.println("Error in the calculation")
    }
  }

  def calcPageRankBasedOnQuery(query: String): Map[(String, List[String]), List[Double]] = {
    ////TODO: map reduce per a filtrar els fitxers que tenen la paraula search/paraules
    val filename ="stopwordscatalanet.txt"; //todo: CHANGE THIS LOL TO A general one.
    val stopWords = ProcessListStrings.llegirFitxer(filename);
    val stopWordsSet = Viqui.normalize(stopWords).toSet

    val myQueryFiltered = Viqui.normalize(query).filterNot(word => stopWordsSet.contains(word))
    val myQuery =  if (myQueryFiltered.isEmpty) Viqui.normalize(query) else myQueryFiltered
    val myQuerySize = Math.min(myQuery.size,1);

    def readFilesPageRank(path: String): (Map[(String,List[String]),List[Double]],Int) = { //TODO: Map[(String,List[String]) -> Map[(String,Array[String])
      val (filesPath, nFiles) = getListOfFiles2(path);
      def mapperReadFiles(file: String, nothing: List[Nothing]): List[((String,List[String]),List[Nothing])] = { //List[Nothing] a List[nFiles]
        List(
          ViquipediaParse.parseViquipediaFile(file) match {
            case ResultViquipediaParsing(titol, cont, refs) =>
              val nonStopCont = Viqui.normalize(cont).filterNot(str => stopWordsSet.contains(str));
              if(nonStopCont.nonEmpty){
              val ngram = Viqui.ngrames(nonStopCont,myQuerySize);
              if(ngram.contains(myQuery)){
                ((titol,refs),List())
              }
              else{
                (("",List[String]()),List())
              }
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
      (MRWrapper.execute(filesPath, mapperReadFiles, reducerReadFiles,nMappers,nReducers),nFiles)
    }
    val (fileCont,nFiles) = readFilesPageRank(viquiFilesPath);
    val keyToRemove = ("", List[String]()) // Adjust according to the actual key type
    val newFileCont = fileCont - keyToRemove
    //println("newFileCont: " + newFileCont);
    PR(newFileCont,nFiles)
  }

  def nonMutualReference(filesPath: String, query: String) = {
    //val (filesContents,nFiles) = getListOfFiles2(viquiFilesPath);
    val pageRank = calcPageRankBasedOnQuery(query);
    val titleRefs = pageRank.map(pr => (pr._1._1,pr._1._2))

    /*
    def mapperReadRefsTitles(filePath: String, nothing: List[Nothing]): List[(String,String)] = {
      ViquipediaParse.parseViquipediaFile(filePath) match {
        case ResultViquipediaParsing(title, _, refs) => refs.map(ref => (ref,title));
      }
    }

    def reducerReadRefsTitles(mainReference: String, titlesReferenced: List[String]): (String,List[String])= {
      //titol unic -> un unic head
      (mainReference,titlesReferenced)
    }
    val refsTitles: Map[String,List[String]] = MRWrapper.execute(filesContents, mapperReadRefsTitles, reducerReadRefsTitles,nMappers,nReducers)
*/

    //def mapperReadTitlesRefs(filePath: String, nothing: List[Nothing]): List[(String,(String,List[String]))] = {
    //  List(ViquipediaParse.parseViquipediaFile(filePath) match {
    //    case ResultViquipediaParsing(title, cont, refs) => (title,(cont,refs))
    //  })
    //}
//
    //def reducerReadTitlesRefs(title: String, contRefs: List[(String,List[String])]): (String,List[(String,List[String])])= {
    //  //titol unic -> un unic head
    //  (title,List(contRefs.head))
    //}
    //val titleRefs: Map[String,List[(String,List[String])]] = MRWrapper.execute(filesContents, mapperReadTitlesRefs, reducerReadTitlesRefs,nMappers,nReducers)


    def mapperSelectNonReferenced(title: String, refsCont: List[String]) = {
      //puc agafar directament head pq per cada titol només hi ha una llista de referencies.
      titleRefs.map(tr =>
        if(tr._2.contains(title) && refsCont.contains(tr._1) || tr._1 == title){
          ("","");
        }
        else {
          (tr._1,title)
        }
      ).toList
    }

    def reducerSelectNonReferenced(title1: String, titlesNonMutuallyReferenced: List[String]) = {
      (title1,titlesNonMutuallyReferenced)
    }

    val nonMutuallyReferenced = MRWrapper.execute(titleRefs.toList,mapperSelectNonReferenced,reducerSelectNonReferenced,nMappers,nReducers);
    val keyToRemove = ("") // Adjust according to the actual key type
    val newNonMutuallyReferenced = nonMutuallyReferenced - keyToRemove
    newNonMutuallyReferenced
  }
/*
  def cosinesim(input: String, other: String, stopWords: List[String], n: Int): Float = {

    val stopWordsSet: Set[String] = stopWords.toSet
    var inputFreq: Array[(String,Int)] = null; //això ho faig per a aque faci servir nonstopfreq quan n sigui 1.
    var otherFreq: Array[(String,Int)] = null;
    if(n==1){
      inputFreq = nonstopfreq(input, stopWords).toArray;
      otherFreq = nonstopfreq(other,stopWords).toArray;
    }
    else{
      inputFreq = ngramesNonstopwords(input, n, stopWordsSet).toArray;
      otherFreq = ngramesNonstopwords(other, n, stopWordsSet).toArray;
    }

    //els passo a map per a poder fer les operacoins més rapides.
    //alhora trobo el maxim de la frequencia i construeixo el mapa.
    val (inputMap, maxInputFreq) = inputFreq.foldLeft((Map[String, Int](), 0)) { case ((map, maxFreq), (word, count)) =>
      (map.updated(word, count), math.max(maxFreq, count))
      //acutalitzo el valor del word amb el count que té. I busco pel maxim si el nou count és més gran.
    }

    //faig el mateix per a l'altre.
    val (otherMap, maxOtherFreq) = otherFreq.foldLeft((Map[String, Int](), 0)) { case ((map, maxFreq), (word, count)) =>
      (map.updated(word, count), math.max(maxFreq, count))
    }

    //trobo totes les paraules uniques.
    val allWords = (inputMap.keys ++ otherMap.keys).toSet;

    //faig que totes les paraules estiguin alineades per a poder fer a[i] · b[i] //posant 0 a on no hi ha valors.
    val aligned:Array[(String,Int,Int)] = allWords.toArray.map { word =>
      val inputCount = inputMap.getOrElse(word,0);
      val otherCount = otherMap.getOrElse(word,0);
      (word,inputCount,otherCount)
    }

    //transformo els pesos a valors relatius.
    val weightedAlignment = aligned.map { case (word,inputCount,otherCount) =>
      val normalizedInput = inputCount.toFloat / maxInputFreq
      val normalizedOther = otherCount.toFloat / maxOtherFreq
      (word,normalizedInput,normalizedOther)
    }

    //calculating the formula
    val resultatNomerador = weightedAlignment.foldLeft(0.toFloat) {(acc,input) => acc + (input._2*input._3)}
    val resultatDenominadorA = Math.sqrt(weightedAlignment.foldLeft(0.toFloat) {(acc,input) => acc + input._2*input._2})
    val resultatDenominadorB = Math.sqrt(weightedAlignment.foldLeft(0.toFloat) {(acc,input) => acc + input._3*input._3})
    val resultat = resultatNomerador/(resultatDenominadorA*resultatDenominadorB);
    resultat.toFloat
  }
*/
  def inverseDocFreq(contingut: Map[String,List[(String,List[String])]], newNonMutuallyReferenced: Map[String, List[String]]): Unit = {


    //cridar el cosinesim per a tots.
  }

  def calcSimilitudDeNoReferenciadesMutuament(query: String): Unit = {
    val start = System.nanoTime()
    val (fileCont,nFiles) = getListOfFiles2(viquiFilesPath);
    def mapperReadTitlesRefs(filePath: String, nothing: List[Nothing]): List[(String,(String,List[String]))] = {
      List(ViquipediaParse.parseViquipediaFile(filePath) match {
        case ResultViquipediaParsing(title, cont, refs) => (title,(cont,refs))
      })
    }

    def reducerReadTitlesRefs(title: String, contRefs: List[(String,List[String])]): (String,List[(String,List[String])])= {
      //titol unic -> un unic head
      (title,List(contRefs.head))
    }
    val contingut: Map[String,List[(String,List[String])]] = MRWrapper.execute(fileCont, mapperReadTitlesRefs, reducerReadTitlesRefs,nMappers,nReducers)


    val nonMutualReferencedFilesContentsMap = nonMutualReference(viquiFilesPath,query);

    inverseDocFreq(contingut,nonMutualReferencedFilesContentsMap);


    //MRWrapper.execute();

    println("tf_idf " + (System.nanoTime()-start)/1000000 + "ms");
  }


  object Menu {

    def mainMenu(): Unit = {
      println()
      println("Main Menu:")
      println("1. Calculate the average number of references that all pages have.")
      println("2. Recommendation based on query.")
      println("3. Detect similar pages that don't reference each other.")
      println("4. Time measurement.")
      println("5. Change file folder.")
      println("5. Exit.")

      val choice = readLine("Please select an option (1-5): ").trim

      choice match {
        case "1" => optionAverageRefs()
        case "2" => optionRecomendation()
        case "3" => similarNonMutualReferenced()
        case "4" => option4()
        case "5" => println("Exiting... Goodbye!")
        case _ =>
          println("Invalid choice, please try again.")
          mainMenu()
      }
    }

    def optionAverageRefs(): Unit = {
      println()
      println("Option 1: Calculate the average number of references that all pages have.")
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
              val pageRank = calcPageRankBasedOnQuery(query);
              val orderedPG = pageRank.toList.sortWith(_._2.sum > _._2.sum).take(10);
              println("Top 10: ")
              orderedPG.foreach( f => printf("%s %.10f\n", f._1._1, f._2.sum))
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

    def similarNonMutualReferenced(): Unit = {
      println()
      println("Option 3: Detect similar pages that don't reference each other.")
      println("1. Start.")
      println("2. Go Back to Main Menu")

      val choice = readLine("Please select an option (1-2): ").trim

      choice match {
        case "1" =>
          calcSimilitudDeNoReferenciadesMutuament("text");
          mainMenu()
        case "2" => mainMenu()
        case _ =>
          println("Invalid choice, please try again.")
          similarNonMutualReferenced()
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