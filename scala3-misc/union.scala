case class UserName(name: String)
case class Password(hash: Int)

type U = UserName | Password

// r is not automatically inferred to be `UserName | Password`. It defaults to
// Object!
val r1: U = if true then UserName("Fred") else Password(123)
val r2: U = if false then UserName("Fred") else Password(123)
