import java.io.File

import udg.objects.Viqui

object pra2 extends App {
  def getListOfFiles(path: String): List[String] = {
    val files = new File(path);
    files.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).map(f => f.getPath.mkString).toList;
  }
  println("Start")

  val viquiFilesPath = "viqui_files/";
  val viqui_files = getListOfFiles(viquiFilesPath);

  //println(viqui_files);
  println(viqui_files.head)

  Viqui.parseXMLFile(viqui_files.head);
}