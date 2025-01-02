module Day25 where

import Prelude

import Data.Array (all, concatMap, filter, fromFoldable, head, length, partition, reverse, takeWhile, transpose, zipWith)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.Traversable (sequence)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (choice, sepBy, try)
import Parsing.Combinators.Array (many, many1)
import Parsing.String (anyTill, char, string)
import Partial (crashWith)

main ∷ Effect Unit
main = run 25 (Just (replaceAll (Pattern "\n\n") (Replacement " "))) parser (Right <<< calc)

calc pinss =
  let
    { yes: locksTransposed, no: keysTransposed } = partition
      ( \pins -> head pins
          <#> all identity
          # fromMaybe false
      )
      pinss
    keys = spyWith "" show $ keysTransposed <#> reverse <#> transpose
    locks = spyWith "" show $ locksTransposed <#> transpose
  in
    concatMap (\key -> map (\lock -> fits key lock) locks) keys
      # filter identity
      # length

fits :: Pins -> Pins -> Boolean
fits key lock =
  let
    trueLengths pins = pins <#> (takeWhile identity >>> length)
  in
    zipWith (+) (trueLengths key) (trueLengths lock)
      # all (_ < 8)

type Pins = Array (Array Boolean)

parser :: Parser String (Array Pins)
parser = pins `sepBy` string " " <#> fromFoldable

pins :: Parser String Pins
pins = (many1 $ choice [ char '#' $> true, char '.' $> false ]) `sepBy` string "\n" <#> (_ <#> fromFoldable) <#> fromFoldable