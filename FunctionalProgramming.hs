--
-- Inspired by http://vimeo.com/20290504
--

import Data.List (foldl')
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

(|>) = flip ($)

thing = [1..1000000]
    |> map (* 2)
    |> filter (> 100000)
    |> foldl' (+) 0

--  
-- See http://www.haskell.org/haskellwiki/Timing_computations
--
timeOnce f = do
  start <- getCPUTime
  result <- f
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return result

main = do
  a <- timeOnce $ thing `seq` return ()
  print a
{-
  def timeStamp = System.currentTimeMillis

  def timeOnce(f: => Unit) = {
    val start = timeStamp
    f
    timeStamp - start
  }

  implicit def implicitTimes(n: Int) = new {
    def times(f: => Unit) = (1 to n).foreach { ignore =>
      f
    }
  }

  def main(args: Array[String]) {
    10.times {
      println(timeOnce(thing).toFloat / 1000)
    }
  }
}
-}
