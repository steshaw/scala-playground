case class Building(start: Int, end: Int, height: Int)

def lowerThan(height: Int) = (b: Building) => b.height < height

val lowerThanSix = lowerThan(6)

val result = lowerThanSix(Building(1, 2, 5))

assert(result == true)
Console.println("ok")
