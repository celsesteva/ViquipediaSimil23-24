package udg.objects
object Viqui {
    def myFoldLists(words: List[List[String]]): Map[List[String],Int] = {
    words.foldLeft(Map[List[String], Int]()) { (acc: Map[List[String],Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // accumula la cuantitat de words que ha trobat
      } else {
        acc.updated(word, 1) //Si és el primer cop que la troba, l'afageix amb 1.
      }
    }
  }

  def myFoldWords(words: List[String]): Map[String,Int] = {
    words.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // accumula la cuantitat de words que ha trobat
      } else {
        acc.updated(word, 1) //Si és el primer cop que la troba, l'afageix amb 1.
      }
    }
  }

  def ngrames(input: List[String], n: Int): Map[List[String], Int] = { //it removes numbers
    val slidingWindows = input.sliding(n).toList
    val wordCounts: Map[List[String], Int] = Viqui.myFoldLists(slidingWindows)
    wordCounts
  }

  def normalize(input: String): List[String] = {
    input.map(c => if(c.isLetter || c.isWhitespace) c.toLower else if (c.isDigit) c else ' ').split("\\s+").filter(_.nonEmpty).toList;
  }
}
/*
def readFilesRefs(path: String): (Map[(String,List[String]),List[Double]],Int) = {
  val (filesPath, nFiles) = getListOfFiles2(path);
  def mapperReadFilesRef(file: String, nothing: List[Nothing]): List[((String,List[String]),List[Nothing])] = { //List[Nothing] a List[nFiles]
    List(
      ViquipediaParse.parseViquipediaFile(file) match {
        case ResultViquipediaParsing(titol, _, refs) => ((titol,refs),List())
        case _ => throw new Exception("Error en el readFilesRefs.")
      })
  }

  def reducerReadFilesRef(titol: (String,List[String]), nothing: List[List[Nothing]]): ((String,List[String]),List[Double]) = { //només hi ha un double per (titol,refs)
    (titol,List(1.0/nFiles))
  }

  (MRWrapper.execute(filesPath, mapperReadFilesRef, reducerReadFilesRef,1,1),nFiles)
}

  //val (viqui,nFilessss) = getListOfFiles2(viquiFilesPath);
  //println(nFilessss)
  //println(viqui);



def readFiles(path: String): (Map[(String,String,List[String]),List[Double]],Int) = {
    val (filesPath, nFiles) = getListOfFiles2(path);
    def mapperReadFiles(file: String, nothing: List[Nothing]): List[((String,String,List[String]),List[Nothing])] = { //List[Nothing] a List[nFiles]
      List(
        ViquipediaParse.parseViquipediaFile(file) match {
          case ResultViquipediaParsing(titol, cont, refs) => ((titol,cont,refs),List())
          case _ => throw new Exception("Error en el readFiles.")
        })
    }

    def reducerReadFiles(titol: (String,String,List[String]), nothing: List[List[Nothing]]): ((String,String,List[String]),List[Double]) = { //només hi ha un double per (titol,refs)
      (titol,List(1.0/nFiles))
    }

    (MRWrapper.execute(filesPath, mapperReadFiles, reducerReadFiles,1,1),nFiles)
  }

 */