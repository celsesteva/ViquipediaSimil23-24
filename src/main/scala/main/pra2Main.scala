import java.io.File
import udg.objects.Viqui

import scala.xml.{InputSource, XML}
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes

import org.xml.sax.InputSource
import javax.xml.parsers.SAXParserFactory
import scala.xml.parsing.NoBindingFactoryAdapter
import javax.xml.stream.XMLInputFactory
import java.io.FileInputStream

object pra2 extends App {
  def getListOfFiles(path: String): List[String] = {
    val files = new File(path);
    files.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).map(f => f.getPath.mkString).toList;
  }
  def xmlDocument(document: Option[String]): String = document match {
    case Some(value) => value;
    case _ => "";
  }

  println("Start")
  val viquiFilesPath = "viqui_files/";
  val viquiFiles = getListOfFiles(viquiFilesPath);
  Viqui.parseXMLFile(viquiFiles.head);

  //val xmlFile = XML.load(viquiFiles.head);



  val before = System.nanoTime

  val inputStream = new FileInputStream(viquiFiles.head)
  val factory = XMLInputFactory.newInstance()
  val reader = factory.createXMLStreamReader(inputStream)

  while (reader.hasNext) {
    reader.next() match {
      case javax.xml.stream.XMLStreamConstants.START_ELEMENT =>
        val localName = reader.getLocalName
        if (localName == "title") {
          println(s"Title: ${reader.getElementText}")
        } else if (localName == "text") {
          println(s"Text: ${reader.getElementText}")
        }
      case _ => // Do nothing
    }
  }

  //val title = (xmlFile \ "title").headOption.map(_.text);
  //val text= (xmlFile \ "revision" \ "text").headOption.map(_.text);


  val totalTime = System.nanoTime - before

  //println(xmlDocument(title))
  //println(xmlDocument(text))

  println(s"Time taken: " + totalTime/1000000 + "ms")
}