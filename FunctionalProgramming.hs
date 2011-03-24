--
-- Inspired by http://vimeo.com/20290504
--

import Data.List (foldl')
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

timeStamp :: IO Integer
timeStamp = getCPUTime

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

computation :: Integer
computation = [1..1000000]
    |> map (* 2)
    |> filter (> 100000)
    |> foldl' (+) 0

--
-- See http://www.haskell.org/haskellwiki/Timing_computations
--
timeOnce :: IO a -> IO a
timeOnce f = do
  start <- timeStamp
  result <- f
  end <- timeStamp
  let diff = (fromIntegral (end - start)) / (10^9)
  _ <- printf "Computation time: %0.3fms\n" (diff :: Double)
  return result

{-
  Hmmm, in Haskell, I don't think there's a way to repeat this computation many times to average
  the running times...
-}
{-
  def main(args: Array[String]) {
    10.times {
      println(timeOnce(computation).toFloat / 1000)
    }
  }
}
-}

inIO :: a -> IO a
inIO value = value `seq` return value

main :: IO ()
main = do
  a <- timeOnce $ inIO computation
  putStrLn $ "result => " ++ (show a)
