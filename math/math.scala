
import Math.pow

implicit def anon(n: Int) = new { def **(exp: Int) = pow(n, exp).asInstanceOf[Int] }               

def twos_complement_minmax(bits: Int) = {
  val twos_bits = bits - 1
  val twos_magnitude = 2 ** twos_bits
  (-twos_magnitude, twos_magnitude - 1)
}

List(8, 16, 32, 64, 128) foreach {(i) => {
  println(twos_complement_minmax(i))
}}

import java.lang.Short
import java.lang.Integer
import java.lang.Long

// XXX: Hmmm, what's going on here?

println((twos_complement_minmax(16), (Short.MIN_VALUE, Short.MAX_VALUE)))
println(twos_complement_minmax(16) == (Short.MIN_VALUE, Short.MAX_VALUE))

println((twos_complement_minmax(32), (Integer.MIN_VALUE, Integer.MAX_VALUE)))
println(twos_complement_minmax(32) == (Integer.MIN_VALUE, Integer.MAX_VALUE))

println((twos_complement_minmax(64), (Long.MIN_VALUE, Long.MAX_VALUE)))
println(twos_complement_minmax(64) == (Long.MIN_VALUE, Long.MAX_VALUE))
