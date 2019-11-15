package steshaw.resty;

import com.sun.jersey.api.json.JSONJAXBContext;
import steshaw.resty.User;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.JAXBContext;


/**
 * @See http://blogs.sun.com/enterprisetechtips/entry/configuring_json_for_restful_web
 */
@Provider
public class MyJaxbContextResolver implements ContextResolver<JAXBContext> {

  private JAXBContext context;
  private Class[] types = {User.class};

  public MyJaxbContextResolver() throws Exception {
    Map props = new HashMap<String, Object>();
    props.put(JSONJAXBContext.JSON_NOTATION, JSONJAXBContext.JSONNotation.MAPPED);
    props.put(JSONJAXBContext.JSON_ROOT_UNWRAPPING, Boolean.TRUE);
    props.put(JSONJAXBContext.JSON_ARRAYS, new HashSet<String>(1){{add("jobs");}});
    props.put(JSONJAXBContext.JSON_NON_STRINGS, new HashSet<String>(1){{add("id"); add("@id");}});
    this.context = new JSONJAXBContext(types, props);
  }

  public JAXBContext getContext(Class<?> objectType) {
      return (types[0].equals(objectType)) ? context : null;
  }
}

