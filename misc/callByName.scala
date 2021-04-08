//
// http://stackoverflow.com/questions/38846574/
//

def log(a: Any) = println(a)

def func1(x: => Int) = x * x
def func2(x: => Int) = {
  log("I'm func2!")
  x + 1
}

var a = 0

def rest1(b: => Int) = {
  def rest2(c: => Int) = {
    a = 1
    log(c); // Logs "I'm func2!", and then logs 2, which is ((a * a) + 1).
    log(b); // Logs 1, which is (a * a).

    a = 2;
    log(c); // Logs "I'm func2!", and then logs 5, which is ((a * a) + 1).
    log(b); // Logs 4, which is (a * a).
  }
  rest2(func2(b))
}
rest1(func1(a))
