import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._


object JsonCoder {
  def parseJsonField(jsonFile: String, fieldName: String): List[String] = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    // Чтение JSON из файла
    val jsonString = Source.fromFile(jsonFile).mkString

    // Парсинг JSON
    val json = parse(jsonString)

    // Извлечение значения выбранного поля для каждого объекта в JSON
    val fieldValues = for {
      JObject(obj) <- json
      JField(`fieldName`, JString(value)) <- obj
    } yield value

    fieldValues
  }
}
