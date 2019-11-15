package steshaw.resty.resource;

import javax.ws.rs.GET;
import javax.ws.rs.Produces;
import javax.ws.rs.Path;

@Path("java")
public class JavaResource {

    public static String message = "Java's no fun any more :(";

    @GET
    @Produces("text/plain")
    public String getGreeting() {
        return "Hi there from Java\n";
    }

    @GET
    @Path("/scala")
    @Produces("text/plain")
    public String outputScalaMessage() {
        return "Hi there. " + steshaw.resty.resource.ScalaResource.message() + "\n";
    }

}
