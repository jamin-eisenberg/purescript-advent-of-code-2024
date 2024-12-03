module Day3 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, sum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), snd, uncurry)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (choice, many, manyTill_, sepEndBy, try)
import Parsing.String (anyChar, anyTill, char, satisfy, string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 3 Nothing parser (Right <<< calc)

calc :: List Term -> Int
calc terms =
  terms
    # foldl handleTerm { validMuls: Nil, enabled: true }
    # _.validMuls
    <#> uncurry mul
    # sum

handleTerm { validMuls, enabled } =
  case _ of
    Do -> { validMuls, enabled: true }
    Don't -> { validMuls, enabled: false }
    Mul n1 n2 -> if enabled then { validMuls: Tuple n1 n2 : validMuls, enabled } else { validMuls, enabled }

data Term = Do | Don't | Mul Int Int

parser :: Parser String (List Term)
parser = do
  muls <- many $ try $ anyTill termParser -- (anyChar `manyTill_` mulParser)
  pure (muls <#> snd)

termParser = choice [ mulParser, string "do()" $> Do, string "don't()" $> Don't ]

mulParser = do
  _ <- string "mul("
  n1 <- intDecimal
  _ <- char ','
  n2 <- intDecimal
  _ <- char ')'
  pure $ Mul n1 n2