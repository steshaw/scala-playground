package steshaw.resty.resource

import steshaw.resty.User
import java.util.Date
import javax.ws.rs.{GET, Produces, Path, PathParam}

@Path("/scala")
class ScalaResource {

  @GET
  @Produces(Array("text/plain"))
  def helloScala = "RESTful hello from Scala!\n";

  @GET
  @Path("/java")
  def java = "Hello. " + steshaw.resty.resource.JavaResource.message + "\n"

  @GET
  @Path("/user/{id}")
  @Produces(Array("application/json", "application/xml"))
  def blah(@PathParam("id") id:String) = User(id, "Steven Shaw", "steshaw@gmail.com")

  val name = "Steve"

  //
  // See https://jersey.dev.java.net/source/browse/jersey/trunk/jersey/samples/scala-helloworld-webapp/src/main/scala/com/sun/jersey/samples/helloworld/resources/MarkupResource.scala?annotate=2305
  //
  @GET
  @Path("/user/steve")
  @Produces(Array("text/html"))
  def get =
    <html>
      <body>
        <h1>Hello {name}</h1>
        <p>Generated at {new Date()}</p>
      </body>
    </html>
}

object ScalaResource {
  val message = "Scala rocks!"
}
