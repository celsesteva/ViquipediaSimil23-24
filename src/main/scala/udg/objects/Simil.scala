package udg.objects

import scala.::
import scala.annotation.tailrec

object Simil {
  def normalize(input: String): List[String] = {
    val inputLetters = input.toLowerCase.map(c => if(c.isLetter || c.isWhitespace) c else ' ').split("\\s+").toList.filter(_.nonEmpty);
    inputLetters
  }

  def freq(input: String): List[(String,Int)] = {
    val inputNormalized = normalize(input);
    val wordCounts: Map[String, Int] = inputNormalized.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // Update the word count
      } else {
        acc.updated(word, 1) // Insert new word with count 1
      }
    }
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  @tailrec
  private def printFreqResults(maxWords: Int, input: List[(String,Int)], n: Int): Unit = (maxWords,input,n) match {
    case (0,_,_) => println("There are no words");
    case (_,_,0) => println();
    case (maxWords,head :: tail,n)  =>
      val (word,times) = head;
      val freq = BigDecimal(times.toFloat/maxWords*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat;
      println(f"$word%-20s$times%-10d$freq%-10s")
      printFreqResults(maxWords,tail,n-1);
    case _ => println("Error")
  }

  def printResultsFreq(input: String): Unit = {
    val aliceInWonderlandContent = normalize(input);
    println("Num de Paraules:\t" + aliceInWonderlandContent.size + "\tDiferents:\t" + aliceInWonderlandContent.distinct.size)
    println("Paraules\tocurrencies\tfrequencia")
    println("----------------------------------")
    printFreqResults(aliceInWonderlandContent.size,freq(input).sortWith(_._2>_._2), 10);
  }

  def printResultsNonStopWords(input: String, stopWords: List[String]): Unit = {
    val aliceInWonderlandContent = normalize(input);
    val stopWordsSet: Set[String] = stopWords.toSet
    val nonstop = aliceInWonderlandContent.filterNot(c => stopWordsSet.contains(c));
    println("Num de Paraules:\t" + nonstop.size + "\tDiferents:\t" + nonstop.distinct.size)
    println("Paraules\tocurrencies\tfrequencia")
    println("----------------------------------")
    printFreqResults(aliceInWonderlandContent.size,nonstopfreq(input,stopWords).sortWith(_._2>_._2), 10);
  }

  def nonstopfreq(input: String, stopWords: List[String]): List[(String,Int)] = {
    val inputNormalized = normalize(input);
    val stopWordsSet: Set[String] = stopWords.toSet
    val nonstop = inputNormalized.filterNot(c => stopWordsSet.contains(c));

    val wordCounts: Map[String, Int] = nonstop.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1)
      } else {
        acc.updated(word, 1)
      }
    }
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  @tailrec
  private def printFreq(input: List[(Int,Int)], n: Int): Unit = (input,n) match {
    case (_,0) => println();
    case (head :: tail, n) =>
      val (times, words) = head;
      println(s"$words paraules apareixen $times vegades");
      printFreq(tail,n-1);
    case _ => ()
  }

  def paraulafreqfreq(input: String): Unit = {
    val words = freq(input);
    val frequencies = words.groupBy(_._2).map(input => (input._1,input._2.size)).toList.sortWith((a,b) => a._2 > b._2 || (a._2 == b._2 && a._1 < b._1)); //.sortWith((a,b) => a._2 > b._2), sortWith {case  ((_,nu1), (_,nu2)) => nu1 > nu2}

    println("Les 10 frequencies mes frequents:")
    printFreq(frequencies.take(10),10);
    println("Les 5 frequencies menys frequents:")
    printFreq(frequencies.reverse.take(5),5);
  }

  def printWindow(input: List[(String,Int)], n: Int): Unit = (input,n) match {
    case (_,0) => ()
    case (head :: tail, n) =>
      val (words, count) = head;
      println((f"$words%-40s $count%-40d"))
      printWindow(tail,n-1);
    case _ => ()
  }

  def ngrames(input: String, n: Int): List[(String, Int)] = {
    val words = normalize(input);
    val slidingWindows = words.sliding(n).toList
    val flattened = slidingWindows.map(input => input.mkString(" "))

    val wordCounts: Map[String, Int] = flattened.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // Update the word count
      } else {
        acc.updated(word, 1) // Insert new word with count 1
      }
    }
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  def printNgram(input: List[(String,Int)],n: Int): Unit = (input,n) match {
    case (_,0) => println()
    case (head :: tail, n) =>
      val (words, count) = head;
      println((f"$words%-40s $count%-40d"))
      printNgram(tail,n-1);
    case _ => ()
  }

  def ngramesNonstopwords(input: String, n: Int,stopWordsSet: Set[String]): List[(String, Int)] = {
    val words = normalize(input);
    val nonstop = words.filterNot(c => stopWordsSet.contains(c));

    val slidingWindows = nonstop.sliding(n).toList
    val flattened = slidingWindows.map(input => input.mkString(" "))

    val wordCounts: Map[String, Int] = flattened.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // Update the word count
      } else {
        acc.updated(word, 1) // Insert new word with count 1
      }
    }
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  def cosinesim(input: String, other: String, stopWords: List[String], n: Int): Float = {
    val before = System.nanoTime;
    val stopWordsSet: Set[String] = stopWords.toSet
    var inputFreq: Array[(String,Int)] = null;
    var otherFreq: Array[(String,Int)] = null;
    if(n==1){
      inputFreq = nonstopfreq(input, stopWords).toArray;
      otherFreq = nonstopfreq(other,stopWords).toArray;
    }
    else{
      inputFreq = ngramesNonstopwords(input, n, stopWordsSet).toArray;
      otherFreq = ngramesNonstopwords(other, n, stopWordsSet).toArray;
    }



    val (inputMap, maxInputFreq) = inputFreq.foldLeft((Map[String, Int](), 0)) { case ((map, maxFreq), (word, count)) =>
      (map.updated(word, count), math.max(maxFreq, count))
    }

    val (otherMap, maxOtherFreq) = otherFreq.foldLeft((Map[String, Int](), 0)) { case ((map, maxFreq), (word, count)) =>
      (map.updated(word, count), math.max(maxFreq, count))
    }

    val allWords = (inputMap.keys ++ otherMap.keys).toSet;

    //aligning words
    val aligned:Array[(String,Int,Int)] = allWords.toArray.map { word =>
      val inputCount = inputMap.getOrElse(word,0);
      val otherCount = otherMap.getOrElse(word,0);
      (word,inputCount,otherCount)
    }

    //weighting the alignment
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

    val totalTime=System.nanoTime-before
    println("Time " + totalTime / 1000000 + "ms")

    resultat.toFloat
  }
}