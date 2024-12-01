module Day1 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Function (on)
import Data.List (List, filter, length, sort, unzip, zipWith)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Partial (crashWith)

main ∷ Effect Unit
main = run 1 Nothing parser (Right <<< calc)

calc :: (List (Tuple Int Int)) -> Int
calc rows =
  let
    Tuple l1 l2 = unzip rows
    occurrenceCount x = filter (_ == x) >>> length
  in
    map (\x -> x * occurrenceCount x l2) l1
      # sum

parser ∷ Parser String (List (Tuple Int Int))
parser = row `sepBy` (string "\n")

row = do
  n1 <- intDecimal
  skipSpaces
  n2 <- intDecimal
  pure $ Tuple n1 n2