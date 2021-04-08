//
// From Melbourne Scala User Group thread here:
//
// https://groups.google.com/d/topic/scala-melb/CujaAxmVhQc/discussion
//

{
  // Original expression of interest.
  val result = (for (f <- List(true, true, false)) yield {
    if(f) {
      Some(1)
    } else {
      None
    }
  }).flatten

  println(result)
}

{
  // Hmmm, what's the result before 'flatten'?
  val result = (for (f <- List(true, true, false)) yield {
    if(f) {
      Some(1)
    } else {
      None
    }
  })

  println(result)
  println(result.flatten)
}

{
  // Desugars to 'map'.
  val result = List(true, true, false).map(f => if (f) Some(1) else None)
  println(result)
}

{
  // Avoiding 'flatten' with 'collect'.
  val result = List(true, true, false) collect {case true => 1}
  println(result)
}
