package mr

import java.io.FileInputStream
import javax.xml.stream.XMLInputFactory
import scala.util.matching.Regex
import scala.xml.{Elem, XML}

object ViquipediaParse {

  // Fixem el fitxer xml que volem tractar a l'exemple
  val exampleFilename="viqui_files/32509.xml"

  // Definim una case class per a retornar diversos valor, el titol de la pàgina, el contingut i les referències trobades.
  // El contingut, s'ha de polir més? treure refs? stopwords?...
  case class ResultViquipediaParsing(titol: String, contingut: String, refs: List[String])

  def testParse= this.parseViquipediaFile(exampleFilename)

  def parseViquipediaFile(filename: String=this.exampleFilename) = {
    val inputStream = new FileInputStream(filename)
    val factory = XMLInputFactory.newInstance()
    val reader = factory.createXMLStreamReader(inputStream)
    var contingut: String = null;
    var titol: String = null;


    while (reader.hasNext) {
      reader.next() match {
        case javax.xml.stream.XMLStreamConstants.START_ELEMENT =>
          val localName = reader.getLocalName
          if (localName == "title") {
            titol = reader.getElementText;
          } else if (localName == "text") {
            contingut = reader.getElementText;
          }
        case _ => // Do nothing
      }
      reader.close();
    }




    //val xmlleg = new java.io.InputStreamReader(new java.io.FileInputStream(filename), "UTF-8")

    //// Agafo el document XML i ja està internament estructurat per anar accedint als camps que volguem
    //val xmllegg: Elem = XML.load(xmlleg)

    //// obtinc el titol
    //val titol = (xmllegg \\ "title").text

    //// obtinc el contingut de la pàgina
    //val contingut = (xmllegg \\ "text").text

    // identifico referències
    val ref = new Regex("\\[\\[[^\\]]*\\]\\]")
    //println("La pagina es: " + titol)
    //println("i el contingut: ")
    //println(contingut)
    val refs = (ref findAllIn contingut).toList

    val disallowedChars = Set(':', '#')
    val disallowedPattern = "[:#]".r

    // elimino les que tenen :
    //val filteredRefs = refs.filterNot(ref => disallowedChars.exists(c => ref.contains(c)))
    val filteredRefs = refs.filterNot(ref => disallowedPattern.findFirstIn(ref).isDefined).filterNot(_.isEmpty)

    //elimino [[, | i ]]
    val cleanedRefs = filteredRefs
      .map(ref => ref.split("\\[\\[|\\]\\]|\\|"))
      .collect { case parts if parts.length > 1 => parts(1) } // Only collect if there's a second part
      .filterNot(_.equals(titol))
      .distinct
    //PETA EN EL EXEMPLE exampleFilename.
    //FILTERNOT: he de treure el titol?

    // caldrà eliminar-ne més?

    //for (r <- refs) println(r)
    //println(refs.length)
    //println(filteredRefs.length)
    //xmlleg.close()
    ResultViquipediaParsing(titol, contingut, cleanedRefs)
  }
}