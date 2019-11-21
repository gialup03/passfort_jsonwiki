import upickle.default._
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object WikiInterface{ /*  to run this command you want to type something
                          like :

                          $ sbt "run GET /documents"
                          */
  val fileName = "wiki.json"

  val titleTooLongException = new Exception(
    """
    Title must be fewer than 51 characters long.
    """
  )
  val inputFormException = new Exception(
    """
    Input must take the form :
      |  GET /documents
      |  GET /documents/<title>
      |  POST /documents/<title>
    """
  )
  val newInputException = new Exception(
    """
    new Content must take the form:
      |  { "<new title>" : "<new content...>" }
    """
  )
  val titleNotFoundException = new Exception(
    """
    title not found in wiki
    """
  )

  /** Updates the wiki file to look like d converted to JSON*/
  def updateWiki(d: Map[String,String]) = {
    val wikiObject = new File(fileName)
    val printWriter = new PrintWriter(wikiObject)
    printWriter.write(write(d))
    printWriter.close()
  }

  def main(args: Array[String]) = {
    val n = args.length
    if (n!=2) throw inputFormException

    //import json from the file
    val jDict: String = Source.fromFile(fileName).getLines.mkString
    //convert the json into a scala Map
    var dict: Map[String,String] = read[Map[String,String]](jDict)

    try{
      //CASE: GET /documents
      if (args(0)=="GET" && args(1)=="/documents") {
        println(write(dict.keys))
      }
      //CASE: GET /documents/<title>
      else if (args(0)=="GET" && args(1).take(11)=="/documents/") {
        val key = args(1).substring(11,args(1).length)
        if (dict contains key) println(write(dict(key)))
        else throw titleNotFoundException
      }
      //CASE: POST /documents/<title>
      else if (args(0)=="POST" && args(1).take(11)=="/documents/") {
        val oldKey = args(1).substring(11,args(1).length)
        try {
          println("Please input new content in JSON format.")

          val newContentJson = scala.io.StdIn.readLine()

          val newContent = read[Map[String,String]](newContentJson)

          val newKeySet = newContent.keySet

          require(newKeySet.size==1)

          val newKey = newKeySet.head

          dict -= oldKey
          dict += (newKey->newContent(newKey))
          println(write(dict)) //this returns the whole new wiki to the user, can cut if needed.
          updateWiki(dict)
        }
        catch {case _ : Throwable => throw newInputException}
      }
      else throw inputFormException
    }
    catch {
      case titleTooLongException  : Exception => throw titleTooLongException
      case newInputException  : Exception     => throw newInputException
      case titleNotFoundException : Exception => throw titleNotFoundException
      case _ : Throwable                      => throw inputFormException
    }
  }
}
