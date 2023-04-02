import System.IO
import Control.Monad
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main = do
  input <- readFile "input.txt"
  let doParse = runParser runParse "" input
  print doParse
  let results = getResults input
  print results 

getResults input = getPosition $ foldl runSub initPosition $ getParse
  where
    Right getParse = runParser runParse "" input

data Dir =
    Forward Int
  | Down Int
  | Up Int
  deriving (Eq, Show)

type Parser = Parsec Void String

parseDir :: Parser Dir
parseDir = 
      Forward <$> (string "forward " *> decimal)
  <|> Down <$> (string "down " *> decimal)
  <|> Up <$> (string "up " *> decimal)

runParse :: Parser [Dir]
runParse = many (parseDir <* eol) <* eof

data Position =
  Position {
    x :: Int,
    y :: Int,
  aim :: Int
  }
     deriving (Show, Eq)

initPosition :: Position
initPosition = Position 0 0 0

runSub :: Position -> Dir -> Position
runSub p d = case d of
  Forward n -> (p {x = x p + n})
  Down n -> (p {y = y p + n})
  Up n -> (p {y = y p - n})

getPosition :: Position -> Int
getPosition p = x p * y p
