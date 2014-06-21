import java.net.URLEncoder

import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import apidoc.models._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

@RunWith(classOf[JUnitRunner])
class OrganizationSpec extends ClientSpec {
  "Organizations" should {
    "create an organization" in prop { (name: String) =>
      withClient { client =>
        result {
          client.Organizations.get(name = name).map[Unit] {
            case client.Response(List(o: Organization), 200) => {
              client.Organizations.deleteByGuid(o.guid).entity
            }
            case client.Response(Nil, 200) => ()
          }
        }

        val response = client.Organizations.post(name)
        response.entity.name must equalTo(name)
      }
    }
  }
}
