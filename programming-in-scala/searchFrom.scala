
val args = List("-one.scala", "one.scala", "-two.scala", "two.scala")

def findFirstIndexRecursive(args: Seq[String]): Int = {
  def searchFrom(i: Int): Int =
    if (i >= args.length) -1
    else if (args(i).startsWith("-")) searchFrom(i + 1)
    else if (args(i).endsWith(".scala")) i
    else searchFrom(i + 1)
  searchFrom(0)
}

val fi = findFirstIndexRecursive(args)

// Can we make this functional variant efficient? i.e. sufficiently lazy.
def findFirstIndexFunctional(args: Seq[String]): Int =
  args.zip(0 to 10000).
    filter (t => t match {case (file, index) => !file.startsWith("-") && file.endsWith(".scala")}).
    map (_._2).
    head

val fi2 = findFirstIndexFunctional(args)

println((fi, fi2))
