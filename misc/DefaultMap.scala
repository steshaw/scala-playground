//
// Inspired by scala-debate thread:
//
//   https://groups.google.com/d/topic/scala-debate/BZyygwj7DjQ/discussion
//
// The perhaps surprising difference is the what the get method returns (and therefore contains).
//

import scala.collection.immutable.Map.EmptyMap

var mapWithDefaultValue = Map[String, String]() withDefaultValue "bippy"

var mapWithDefault = Map[String, String]() withDefault (key => "bippy")

var mapOverridesDefault = new EmptyMap[String, String] {
  override def default(key: String): String = "bippy"
}

var mapOverridesApply = new EmptyMap[String, String] {
  override def apply(key: String): String = get(key).getOrElse("bippy")
}

assert (mapWithDefaultValue.apply("hi") == "bippy")
assert (mapWithDefaultValue.get("hi") == Some("bippy"))

assert (mapWithDefault.apply("hi") == "bippy")
assert (mapWithDefault.get("hi") == Some("bippy"))

assert (mapOverridesDefault.apply("hi") == "bippy")
assert (mapOverridesDefault.get("hi") == None)

assert (mapOverridesApply.apply("hi") == "bippy")
assert (mapOverridesApply.get("hi") == None)
