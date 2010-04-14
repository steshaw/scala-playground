
// Add Ruby/Perl style '..' method to Char.
// XXX: Pity that the backslashes are required. Would love to use .. as a legitimate method/function name.
implicit def i_do_not_want_to_name_this_fn(a: Char) = new { def `..`(b: Char) = a to b }

// Add Ruby-style Int.times method.
implicit def i_do_not_want_to_name_this_fn(n: Int) = new { def times(fn: => Unit) = (1 to n).foreach(n => fn) }

// Some useful sets of characters to use for password (i.e. random string) generation.
val lowercase_letters = 'a' `..` 'z'
val uppercase_letters = 'A' `..` 'Z'
val digits = '0' `..` '9'
val punctuation_chars = """~`!@#$%^&*()_-_=+[]{}\|;:'"<>,./?"""
val all_chars = uppercase_letters ++ lowercase_letters ++ digits ++ punctuation_chars

import Stream.continually

def random_password(allowed_chars: IndexedSeq[Char] = all_chars, length: Int = 10) =
  continually(allowed_chars((Math.random * allowed_chars.length).asInstanceOf[Int])) take length mkString

3.times {
  println(random_password())  // The round braces are (unfortunately?) required here because of the default arguments I guess
}

3.times {
  println(random_password(length = 8))
}

3.times {
  println(random_password(allowed_chars = "abc" ++ "123", length = 8))
}
