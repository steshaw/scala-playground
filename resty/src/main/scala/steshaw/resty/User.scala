package steshaw.resty

import javax.xml.bind.annotation._

@XmlRootElement(name = "user")
@XmlAccessorType(XmlAccessType.FIELD)
case class User(@XmlAttribute val id:String, @XmlAttribute val name:String, @XmlAttribute val email:String) {
  def this() = this("", "", "")
}
