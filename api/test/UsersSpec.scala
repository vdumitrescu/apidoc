import java.net.URLEncoder

import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import apidoc.models._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class UsersSpec extends ClientSpec {
  "Users" should {
    "create a user" in prop { (email: String, name: Option[String], imageUrl: Option[String]) =>
      withClient { client =>
        client.Users.get(email = email).entity match {
          case List(u: User) => true
          case Nil => {
            val user = client.Users.post(
              email = email,
              name = name,
              imageUrl = imageUrl
            ).entity
            user.email must equalTo(email)
            user.name must equalTo(name)
            user.imageUrl must equalTo(imageUrl)
            true
          }
        }
      }
    }
  }
}
