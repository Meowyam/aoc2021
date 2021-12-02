import System.IO
import Control.Monad

main = do
  input <- readFile "input.txt"
  let toInt = [ read i :: Integer | i <- words input]
  let result = seeNext toInt []
  print "day 1 part 1"
  print $ length result
  print "day 1 part 2"
  let result2 = seeNext (windows toInt []) []
  print $ length result2

seeNext (x:y:xs) l
  | y > x = seeNext (y:xs) $ l ++ [y:xs]
  | otherwise = seeNext (y:xs) l
seeNext _ l = l

windows (x:y:z:xs) size = windows (y:z:xs) $ size ++ [x+y+z]
windows _ size = size
