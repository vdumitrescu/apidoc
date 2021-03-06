package core

import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSpec}
import org.scalatest.Matchers

class ModelResolverSpec extends FunSpec with Matchers {

  private val guidField = InternalField(name = Some("guid"), fieldtype = Some("uuid"))
  private val userField = InternalField(name = Some("user"), fieldtype = Some("user"))

  private val user = InternalModel(name = "user",
                                   plural = "users",
                                   description = None,
                                   fields = Seq(guidField))

  private val organization = InternalModel(name = "organization",
                                           plural = "organizations",
                                           description = None,
                                           fields = Seq(guidField))

  private val account = InternalModel(name = "account",
                                      plural = "accounts",
                                      description = None,
                                      fields = Seq(userField))

  it("no references") {
    val internal = Seq(user)
    ModelResolver.build(internal).map(_.name) should be(Seq("user"))
  }

  it("multiple models w/ no references") {
    val internal = Seq(user, organization)
    ModelResolver.build(internal).map(_.name).sorted should be(Seq("organization", "user"))
  }

  it("w/ a reference") {
    ModelResolver.build(Seq(account, user)).map(_.name).sorted should be(Seq("account", "user"))
    ModelResolver.build(Seq(user, account)).map(_.name).sorted should be(Seq("account", "user"))
  }

  it("throws error on circular reference") {
    val foo = InternalModel(name = "foo",
                            plural = "foos",
                            description = None,
                            fields = Seq(InternalField(name = Some("guid"), fieldtype = Some("uuid")),
                                         InternalField(name = Some("bar"), fieldtype = Some("bar"))))

    val bar = InternalModel(name = "bar",
                            plural = "bars",
                            description = None,
                            fields = Seq(InternalField(name = Some("guid"), fieldtype = Some("uuid")),
                                         InternalField(name = Some("foo"), fieldtype = Some("foo"))))

    // TODO: Scala test validation of exception
    try {
      ModelResolver.build(Seq(user, account, foo, bar))
      fail("No error raised")
    } catch {
      case e: Throwable => {
        e.getMessage should be("Circular dependencies found while trying to resolve references for models: foo bar")
      }
    }
  }

}
