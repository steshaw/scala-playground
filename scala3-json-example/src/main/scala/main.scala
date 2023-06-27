import upickle.default.ReadWriter as Codec
import upickle.default.{write as writeJson, read as readJson}

enum PetKind derives Codec:
  case Cat, Dog

case class Pet(
  name: String,
  kind: PetKind
) derives Codec // enable coding Pet to and from text

@main def main =
  import PetKind.*
  val coco = Pet(name = "Coco", kind = Cat)
  val message = writeJson(coco)
  println(message)
  val pet = readJson[Pet](message)
  println(pet)
