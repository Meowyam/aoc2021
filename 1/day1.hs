import Data.List
import System.IO
import Control.Monad

main = do
  input <- readFile "input.txt"
  let toInt = [ read i :: Integer | i <- words input]
  let result = seeNext toInt 0
  print "day 1 part 1"
  print result
  print "day 2 part 2"
  let result2 = seeNext (windows toInt []) 0
  print result2

seeNext (x:y:xs) n | y > x = seeNext (y:xs) (n + 1)
seeNext (x:y:xs) n = seeNext (y:xs) n
seeNext _ n = n

windows (x:y:z:xs) size = windows (y:z:xs) $ size ++ [x+y+z]
windows _ size = size



