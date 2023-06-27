object Union:
  def main(args: Array[String]) =
    case class UserName(name: String)
    case class Password(hash: Int)

    // r is not automatically inferred to be `UserName | Password`. It defaults to Object!
    val r0 = if true then UserName("Fred") else Password(123)
    println(r0)

    type U = UserName | Password
    val r1: U = if true then UserName("Fred") else Password(123)
    println(r1)
    val r2: U = if false then UserName("Fred") else Password(123)
    println(r2)
