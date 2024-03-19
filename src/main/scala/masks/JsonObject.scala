package masks

import org.json4s.JString

case class JsonObject(name: String = "undefined",
                      id: String = "undefined",
                      nametype: String = "undefined",
                      recclass: String = "undefined",
                      mass: Option[String] = None,
                      fall: String = "undefined",
                      year: Option[String] = None,
                      reclat: Option[String] = None,
                      reclong: Option[String] = None,
                      geolocation: Option[Geolocation] = None)


