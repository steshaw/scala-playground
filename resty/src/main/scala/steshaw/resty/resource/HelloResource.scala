package steshaw.resty.resource

import javax.ws.rs.GET
import javax.ws.rs.Produces
import javax.ws.rs.Path

@Path("hello")
class HelloResource {
  @GET
  @Produces(Array("text/plain"))
  def hello() = "Hello from Scala! Blah\n"
}
