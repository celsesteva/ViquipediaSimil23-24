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