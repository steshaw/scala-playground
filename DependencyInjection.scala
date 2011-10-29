//
// Adapted from https://groups.google.com/d/msg/scala-debate/VA4-qUxiN0I/9uGkbtgYToUJ
//

sealed abstract class Case
case object AllUpperCase extends Case
case object NotAllUpperCase extends Case

def e1 = "HELLO"
def e2(s: String) = s.toUpperCase
def e3(s1: String, s2: String) = s1 == s2
def e4(b: Boolean) = if (b) AllUpperCase else NotAllUpperCase

var a = e1
var b = e2(a)
var c = e3(a, b)
var d = e4(c)
println(d)
