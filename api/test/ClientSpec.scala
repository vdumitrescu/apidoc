import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.specs2.runner._
import org.scalacheck._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

trait ClientSpec extends Specification with ScalaCheck {
  // This implicit let's you unwrap the Future as is done everywhere in this file
  // I will admit, that I discoverred this by accident, but it's still awesome.
  implicit def result[T](future: Future[T]) = Await.result(future, Duration.Inf)

  @inline // cutdown on boilerplate with optional query params
  implicit def x2option[X](x: X): Option[X] = Option(x)

  def withClient[T](f: apidoc.Client => T): T = {
    val apiToken = "ZdRD61ODVPspeV8Wf18EmNuKNxUfjfROyJXtNJXj9GMMwrAxqi8I4aUtNAT6"
    running(TestServer(testServerPort)) {
      val client = new apidoc.Client(s"http://localhost:$testServerPort", Some(apiToken))
      f(client)
    }
  }
}
