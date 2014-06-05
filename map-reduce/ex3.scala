import scala.collection.mutable.ArrayBuffer

type A[T] = ArrayBuffer[T]

case class Building(start: Int, end: Int, height: Int) {
  def width = end - start
}

case class Foo(val initial: Int, val f: (Building, Int) => Int)

def sumHeight = Foo(0, (b: Building, acc: Int) => acc + b.height)
def minWidth   = Foo(Int.MaxValue, (b: Building, acc: Int) => Math.min(b.width, acc))
def maxHeight  = Foo(0, (b: Building, acc: Int) => Math.max(b.height, acc))

def aggregate(bs: A[Building], aggs: List[Foo]): A[Int] = {
  var accs = ArrayBuffer.concat(aggs.map(_.initial))
  bs.foreach ({ b =>
    accs.iterator.zipWithIndex.foreach { case (acc, index) =>
      accs(index) = aggs(index).f(b, accs(index))
    }
  })
  accs
}

// Example usage
val bs: A[Building] = ArrayBuffer(Building(0, 3, 1), Building(0, 4, 1))
//val sumOfHeights :: minOfWidths :: maxOfHeights :: Nil = aggregate(bs, List(sumHeight, minWidth, maxHeight))
val answers = aggregate(bs, List(sumHeight, minWidth, maxHeight))
val sumOfHeights = answers(0)
val minOfWidths = answers(1)
val maxOfHeights = answers(2)
Console.println("heights.sum = " + sumOfHeights)
Console.println("widths.min = " + minOfWidths)
Console.println("heights.maxHeight = " + maxOfHeights)
