package core

import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSpec}
import org.scalatest.Matchers

class ServiceDescriptionDefaultsSpec extends FunSpec with Matchers {

  it("accepts strings and values as defaults for booleans") {
    val json = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc",
      "models": {
        "user": {
          "fields": [
            { "name": "is_active", "type": "boolean", "default": "true" },
            { "name": "is_athlete", "type": "boolean", "default": false }
          ]
        }
      }
    }
    """
    val validator = ServiceDescriptionValidator(json)
    validator.errors.mkString("") should be("")

    validator.serviceDescription.get.models.head.fields.find { _.name == "is_active" }.get.default should be(Some("true"))

    validator.serviceDescription.get.models.head.fields.find { _.name == "is_active" }.get.default should be(Some("true"))
  }

  it("rejects invalid boolean defaults") {
    val json = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc",
      "models": {
        "user": {
          "fields": [
            { "name": "is_active", "type": "boolean", "default": 1 }
          ]
        }
      }
    }
    """
    val validator = ServiceDescriptionValidator(json)
    validator.errors.mkString("") should be("Model[user] field[is_active] Default[1] is not valid for datatype[boolean]")
  }

  it("validates duplicate models in the resources section") {
    val json = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc",
      "models": {
        "user": {
          "fields": [
            { "name": "guid", "type": "uuid" }
          ]
        }
      },
      "resources": [
        {
          "model": "user",
          "operations": [
            {
              "method": "DELETE"
            }
          ]
        },
        {
          "model": "user",
          "operations": [
            {
              "method": "GET"
            }
          ]
        }
      ]
    }
    """
    val validator = ServiceDescriptionValidator(json)
    validator.errors.mkString should be("Model[user] cannot be mapped to more than one resource")
  }

}
