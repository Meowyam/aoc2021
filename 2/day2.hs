import System.IO
import Control.Monad
import Data.Maybe

main = do
  input <- readFile "input.txt"
  let paired = split $ words input
  let forward = getFromFst "forward" (map tupType paired) 0
  let up = getFromFst "up" (map tupType paired) 0
  let down = getFromFst "down" (map tupType paired) 0
  let vertical = down - up
  print "day 2 part 1"
  print $ vertical * forward
  let withAim = eachTup (map tupType paired) (0,0,0) 
  print withAim
  let day2 = trd withAim * scnd withAim  
  print day2

split [] = []
split (x:y:yz) = (x,y) : split yz

tupType :: (String,String) -> (String,Int)
tupType (a,b) = (a, read b :: Int) 

getFromFst :: String -> [(String,Int)] -> Int -> Int
getFromFst str [] n = n
getFromFst str ((a,i):ls) n
  | str == a = getFromFst str ls (i + n)
  | otherwise = getFromFst str ls n

eachTup :: [(String,Int)] -> (Int,Int,Int) -> (Int,Int,Int)
eachTup [] (a,h,d) = (a,h,d)
eachTup ((str,i):ls) (a,h,d)
  | str == "forward" = eachTup ls (a,(i + h),(d + (a*i)))
  | str == "down" = eachTup ls ((a + i),h,d)
  | str == "up" = eachTup ls ((a - i),h,d) 
  | otherwise = eachTup ls (a,h,d)

trd (a,b,c) = c
scnd (a,b,c) = b
