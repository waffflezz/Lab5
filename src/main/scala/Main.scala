import masks.JsonObject
import org.json4s._
import org.json4s.native.JsonMethods._

import scala.io.Source


object Main {
  def searchByField(jsonString: String, fieldName: String, value: String): (Option[JValue], Int) = {
    val json = parse(jsonString)

    def searchNode(node: JValue, steps: Int): (Option[JValue], Int) = {
      node match {
        case JObject(fields) =>
          val (result, newSteps) = fields.foldLeft((Option.empty[JValue], steps)) {
            case ((acc, accSteps), (`fieldName`, JString(fieldValue))) if fieldValue == value =>
              (Some(JString(fieldValue)), accSteps)
            case ((acc, accSteps), (_, fieldValue)) =>
              val (innerResult, innerSteps) = searchNode(fieldValue, accSteps + 1)
              if (innerResult.isDefined) (innerResult, innerSteps)
              else (acc, innerSteps)
          }
          (result, newSteps)
        case JArray(arr) =>
          val (result, newSteps) = arr.foldLeft((Option.empty[JValue], steps)) {
            case ((acc, accSteps), fieldValue) =>
              val (innerResult, innerSteps) = searchNode(fieldValue, accSteps + 1)
              if (innerResult.isDefined) (innerResult, innerSteps)
              else (acc, innerSteps)
          }
          (result, newSteps)
        case _ => (None, steps)
      }
    }

    searchNode(json, 0)
  }

  def main(args: Array[String]): Unit = {
    implicit val formats: Formats = DefaultFormats

    val json = Source.fromFile("D:\\3 курс 2 семестр\\FP\\Lab5\\src\\main\\json3.json").mkString.replace("\\s", "")
    val parsedJson = parse(json)
    val myList = parsedJson.extract[List[JsonObject]]

    val tree = new AVLTree()

    myList.foreach(obj => {
      tree.insert(obj.name.hashCode, obj)
    })

    val (node, count1) = tree.searchWithSteps("Parambu".hashCode)
    println("AVL-tree search")
    println("Value: " + node.get.value)
    println("number of moves: " + count1)

    println("------------------------")

    val (value, count2) = searchByField(json, "name", "Parambu")

    println("Force search")
    println("Value: " + value.get.values)
    println("number of moves: " + count2)
  }
}