module Day3 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd, uncurry)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (many, manyTill_, sepEndBy, try)
import Parsing.String (anyChar, anyTill, char, satisfy, string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 3 Nothing parser (Right <<< calc)

calc :: List (Tuple Int Int) -> Int
calc muls =
  muls
    <#> uncurry mul
    # sum

parser :: Parser String (List (Tuple Int Int))
parser = do
  muls <- many $ try $ anyTill mulParser -- (anyChar `manyTill_` mulParser)
  pure (muls <#> snd)

mulParser = do
  _ <- string "mul("
  n1 <- intDecimal
  _ <- char ','
  n2 <- intDecimal
  _ <- char ')'
  pure $ Tuple n1 n2