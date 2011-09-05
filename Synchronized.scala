//
// Decompile with:
//
//   $ javap -c Synchronized
//
// to see the monitorentry/monitorexit bytecode produced.
//
class Synchronized {
  def synchronized(): Unit = {
    synchronized {
      printf("hi")
    }
  }
}
