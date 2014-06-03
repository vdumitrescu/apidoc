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
    val fields: String = x.fields.map { field =>
      s"""(__ \\ "${field.originalName}").read[${field.datatype.name}]"""
    }.mkString("(", " and\n ", ")")

s"""{
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
${fields.indent}(${name.toLowerCase}.${name}Impl.apply _)
}"""
  }

  def jsonWrites(x: ScalaModel): String = {
    val name = x.name
    val fields: String = x.fields.map { field =>
      s"""(__ \\ "${field.originalName}").write[${field.datatype.name}]"""
    }.mkString("(", " and\n ", ")")
s"""{
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
${fields.indent}(unlift(${name}.unapply))
}"""
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
        require(!p.isOption, "Path parameters cannot be optional.")
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
    val params = op.formParameters
      .map(JsonWritesHelper.writeFun).mkString(",\n\n")
    s"""val payload = play.api.libs.json.Json.obj(
${params.indent}
)"""
  }

  private object JsonWritesHelper {
    def writeFun(body: String)(implicit  d: ScalaDataType): String = {
      s"""{ value: ${d.name} =>
${body.indent}
}"""
    }

    def writeFun(param: ScalaParameter): String = {
      val impl = writeFun(param.datatype)
      s""" "${param.originalName}" -> ($impl)(${param.name})""".trim
    }

    def writeFun(field: ScalaField): String = {
      val impl = writeFun(field.datatype)
      s""" "${field.originalName}" -> ($impl)(value.${field.name})""".trim
    }

    def writeFun(implicit d: ScalaDataType): String = d match {
      case x @ ScalaStringType => "play.api.libs.json.JsString.apply"
      case x @ ScalaIntegerType => "play.api.libs.json.JsNumber(_)"
      case x @ ScalaLongType => "play.api.libs.json.JsNumber(_)"
      case x @ ScalaBooleanType => "play.api.libs.json.JsBoolean(_)"
      case x @ ScalaDecimalType => "play.api.libs.json.JsNumber.apply"
      case x @ ScalaUnitType => throw new UnsupportedOperationException("unsupported attempt to write Unit to json")
      case x @ ScalaUuidType => {
        writeFun("play.api.libs.json.JsString(value.toString)")
      }
      case x @ ScalaDateTimeIso8601Type => {
        writeFun("""
val ts = org.joda.time.format.ISODateTimeFormat.dateTime.print(value)
play.api.libs.json.JsString(ts)
""")
      }
      case x @ ScalaMoneyIso4217Type => ???
      case x: ScalaListType => {
        val innerFun = writeFun(x.inner)
        writeFun(s"""value.map(
${innerFun.indent}
)""")
      }
      case x: ScalaModelType => {
        val fields: String = x.model.fields.map(writeFun).mkString(",\n\n")
        writeFun(s"""play.api.libs.json.Json.obj(
${fields.indent}
)""")
      }
      case x: ScalaOptionType => {
        val innerFun = writeFun(x.inner)
        writeFun(s"""value.map(
${innerFun.indent}
)""")
      }
    }
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
