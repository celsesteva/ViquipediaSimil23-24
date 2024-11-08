package udg.objects

import scala.::
import scala.annotation.tailrec
//TODO: comentar el codi.
object Simil {
  //codi per a transformar les dades al format demanat (sense simbols).
  def normalize(input: String): List[String] = {
    val inputLetters = input.toLowerCase.map(c => if(c.isLetter || c.isWhitespace) c else ' ').split("\\s+").toList.filter(_.nonEmpty);
    inputLetters
  }

  //funció per a evitar repetir codi.
  private def myFold(words: List[String]): Map[String,Int] = {
    words.foldLeft(Map[String, Int]()) { (acc: Map[String,Int], word) =>
      if (acc.contains(word)) {
        acc.updated(word, acc(word) + 1) // accumula la cuantitat de words que ha trobat
      } else {
        acc.updated(word, 1) //Si és el primer cop que la troba, l'afageix amb 1.
      }
    }
  }

  //calcula la frqüència
  def freq(input: String): List[(String,Int)] = {
    val inputNormalized = normalize(input);
    val wordCounts = myFold(inputNormalized)
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  //funció per a fer els prints que demana a l'enunciat.
  @tailrec
  private def printFreqResults(maxWords: Int, input: List[(String,Int)], n: Int): Unit = (maxWords,input,n) match {
    case (0,_,_) => println("There are no words");
    case (_,_,0) => println();
    case (maxWords,head :: tail,n)  =>
      val (word,times) = head;
      //per fer aproximació faig servir BigDecimal.
      val freq = BigDecimal(times.toFloat/maxWords*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat;
      println(f"$word%-20s$times%-10d$freq%-10s")
      printFreqResults(maxWords,tail,n-1);
    case _ => println("Error")
  }

  //per a fer el print de freq
  def printResultsFreq(input: String): Unit = {
    val aliceInWonderlandContent = normalize(input);
    println("Num de Paraules:\t" + aliceInWonderlandContent.size + "\tDiferents:\t" + aliceInWonderlandContent.distinct.size)
    println("Paraules\tocurrencies\tfrequencia")
    println("----------------------------------")
    printFreqResults(aliceInWonderlandContent.size,freq(input).sortWith(_._2>_._2), 10);
  }
  //per a fer el print de nonstopfrq
  def printResultsNonStopWords(input: String, stopWords: List[String]): Unit = {
    val aliceInWonderlandContent = normalize(input);
    val stopWordsSet: Set[String] = stopWords.toSet
    val nonstop = aliceInWonderlandContent.filterNot(c => stopWordsSet.contains(c));
    println("Num de Paraules:\t" + nonstop.size + "\tDiferents:\t" + nonstop.distinct.size)
    println("Paraules\tocurrencies\tfrequencia")
    println("----------------------------------")
    printFreqResults(nonstop.size,nonstopfreq(input,stopWords).sortWith(_._2>_._2), 10);
  }

  //el mateix que freq però treient abans els stopwords. El passo a set per a que sigui més rapid.
  def nonstopfreq(input: String, stopWords: List[String]): List[(String,Int)] = {
    val inputNormalized = normalize(input);
    val stopWordsSet: Set[String] = stopWords.toSet
    val nonstop = inputNormalized.filterNot(c => stopWordsSet.contains(c));
    val wordCounts: Map[String, Int] = myFold(nonstop)
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  //funció per a extreure codi de paraulafreqfreq
  @tailrec
  private def printFreq(input: List[(Int,Int)], n: Int): Unit = (input,n) match {
    case (_,0) => println();
    case (head :: tail, n) =>
      val (times, words) = head;
      println(s"$words paraules apareixen $times vegades");
      printFreq(tail,n-1);
    case _ => ()
  }

  //el tercer print que es demanava
  def paraulafreqfreq(input: String): Unit = {
    val words = freq(input);
    val frequencies = words.groupBy(_._2).map(input => (input._1,input._2.size)).toList.sortWith((a,b) => a._2 > b._2 || (a._2 == b._2 && a._1 < b._1)); //.sortWith((a,b) => a._2 > b._2), sortWith {case  ((_,nu1), (_,nu2)) => nu1 > nu2}
    println(words.groupBy(_._2))
    println(words.groupBy(_._2).map(input => (input._1,input._2.size)));
    //frequencies: primer agrupo words per el numero d'aparicions.
    //despres transformo el hashMap que esta agrupat per el numero d'aparicions i faig que en lloc de ser aparicions -> List((String,apariconis),(String2,aparcions)),...) sigui aparcions -> aparcions.size)
    //per tant obting el numero de aparcions de cada paraula -> total de paraules amb aquest numero d'aparicons.
    //després ordeno.
    println("Les 10 frequencies mes frequents:")
    printFreq(frequencies.take(10),10);
    println("Les 5 frequencies menys frequents:")
    printFreq(frequencies.reverse.take(5),5);
  }

  //no fa res.
  def printWindow(input: List[(String,Int)], n: Int): Unit = (input,n) match {
    case (_,0) => ()
    case (head :: tail, n) =>
      val (words, count) = head;
      println((f"$words%-40s $count%-40d"))
      printWindow(tail,n-1);
    case _ => ()
  }

  //calcula l'ngrama.
  def ngrames(input: String, n: Int): List[(String, Int)] = {
    val words = normalize(input);
    val slidingWindows = words.sliding(n).toList
    val flattened = slidingWindows.map(input => input.mkString(" "))

    //i toran a ser igual que el freq.
    val wordCounts: Map[String, Int] = myFold(flattened)
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  //printNgrama es per a mostrar un exemple de la pràctica.
  def printNgram(input: List[(String,Int)],n: Int): Unit = (input,n) match {
    case (_,0) => println()
    case (head :: tail, n) =>
      val (words, count) = head;
      println((f"$words%-40s $count%-40d"))
      printNgram(tail,n-1);
    case _ => ()
  }

  //he creat aquest ngraesNon stop words per a poder fer l'ngrama sense stop words i també poder-li passar l'string.
  //perquè així els parametres són els que diu a l'enunciat. L'hi passo el set per estalviar-me calcular el set més cops.
  //per la resta és igual a l'ngrames (sliding window + freq).
  def ngramesNonstopwords(input: String, n: Int,stopWordsSet: Set[String]): List[(String, Int)] = {
    val words = normalize(input);
    val nonstop = words.filterNot(c => stopWordsSet.contains(c));
    val slidingWindows = nonstop.sliding(n).toList
    val flattened = slidingWindows.map(input => input.mkString(" "))

    val wordCounts: Map[String, Int] = myFold(flattened)
    val result: List[(String, Int)] = wordCounts.toList
    result
  }

  //cosinesim estaria bé posar que li passi el set.
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
}