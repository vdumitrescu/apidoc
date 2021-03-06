package core.generator

import core._
import Text._

trait Play2Util {
  def jsonReads(x: ScalaModel): String
  def jsonWrites(x: ScalaModel): String
  def queryParams(operation: ScalaOperation): String
  def pathParams(operation: ScalaOperation): String
  def formParams(operation: ScalaOperation): String
}

object Play2Util extends Play2Util {
  import ScalaDataType._

  def jsonReads(x: ScalaModel): String = {
    val name = x.name
    def read(field: ScalaField): String = field.datatype match {
      case x: ScalaDataType.ScalaListType => {
        // if the key is absent, we return an empty
        // list
        s"""readNullable[${x.name}].map { x =>
  x.getOrElse(Nil)
}"""
      }
      case ScalaDataType.ScalaOptionType(inner) => {
        s"readNullable[${inner.name}]"
      }
      case x => {
        s"read[${x.name}]"
      }
    }
    x.fields match {
      case field::Nil => {
        s"""{
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  (__ \\ "${field.originalName}").${read(field)}.map { x =>
    new ${name}(${field.name} = x)
  }
}"""
      }
      case fields => {
        val builder: String = x.fields.map { field =>
          s"""(__ \\ "${field.originalName}").${read(field)}"""
        }.mkString("(", " and\n ", ")")

        s"""{
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
${builder.indent}(${name}.apply _)
}"""
      }
    }
  }

  def jsonWrites(x: ScalaModel): String = {
    val name = x.name

    x.fields match {
      case field::Nil => {
        s"""new play.api.libs.json.Writes[$name] {
  def writes(x: ${name}) = play.api.libs.json.Json.obj(
    "${field.originalName}" -> play.api.libs.json.Json.toJson(x.${field.name})
  )
}"""
      }
      case fields => {
        val builder: String = x.fields.map { field =>
          s"""(__ \\ "${field.originalName}").write[${field.datatype.name}]"""
        }.mkString("(", " and\n ", ")")

        s"""{
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
${builder.indent}(unlift(${name}.unapply))
}"""
      }
    }
  }

  def queryParams(op: ScalaOperation): String = {
    val queryStringEntries: String = op.queryParameters.map { p =>
      s"queryBuilder ++= ${QueryStringHelper.queryString(p)}"
    }.mkString("\n")
    s"""val queryBuilder = List.newBuilder[(String, String)]
${queryStringEntries}"""
  }

  def pathParams(op: ScalaOperation): String = {
    val pairs = op.pathParameters
      .map { p =>
        require(!p.multiple, "Path parameters cannot be lists.")
        p.originalName -> s"(${PathParamHelper.urlEncode(p.datatype)})(${p.name})"
      }
    val tmp: String = pairs.foldLeft(op.path) {
      case (path, (name, value)) =>
        val spec = s"/:$name"
        val from = path.indexOfSlice(spec)
        path.patch(from, s"/$${$value}", spec.length)
    }
    s""" s"$tmp" """.trim
  }

  def formParams(op: ScalaOperation): String = {
    val params = op.formParameters.map { param =>
      s""" "${param.originalName}" -> play.api.libs.json.Json.toJson(${param.name})""".trim
    }.mkString(",\n")
    s"""val payload = play.api.libs.json.Json.obj(
${params.indent}
)"""
  }

  private object PathParamHelper {
    def urlEncode(d: ScalaDataType): String = {
      s"""{x: ${d.name} =>
  val s = ${ScalaDataType.asString(d)}
  java.net.URLEncoder.encode(s, "UTF-8")
}"""
    }
  }


  private object QueryStringHelper {
    def queryString(p: ScalaParameter): String = {
      val (lhs, dt) = p.datatype match {
        case x: ScalaListType => p.name -> x.inner
        case x: ScalaOptionType => p.name -> x.inner
        case x => s"Seq(${p.name})" -> x
      }
      s"""$lhs.map { x =>
  "${p.originalName}" -> (
${queryString(dt).indent(4)}
  )(x)
}"""
    }

    def queryString(d: ScalaDataType): String = {
      s"""{ x: ${d.name} =>
  ${ScalaDataType.asString(d)}
}"""
    }
  }
}
