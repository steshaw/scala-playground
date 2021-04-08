def millis = System.currentTimeMillis

def time[T](f : => T): T = {
  val start = millis
  var result = f
  val duration = millis - start
  Console.printf("took %sms\n", duration)
  result
}
