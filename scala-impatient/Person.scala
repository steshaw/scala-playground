import scala.reflect.BeanProperty

class Person(@BeanProperty var name: String = "", @BeanProperty var age: Int = 0)
