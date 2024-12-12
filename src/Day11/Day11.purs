module Day11 where

import Prelude

import Data.Array (concatMap, fromFoldable, last, length)
import Data.Either (hush, note)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String as String
import Data.Unfoldable1 (iterateN)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (runParser)
import Parsing.Combinators (sepBy)
import Parsing.String.Basic (intDecimal, number, space)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 11 Nothing parser (note "empty stones" <<< calc)

calc stones =
  iterateN 76 (concatMap blink) stones
    # last
    <#> length

blink :: Number -> Array Number
blink stone =
  let
    stoneLength = (String.length $ show stone) - 2
  in
    case stone of
      0.0 -> [ 1.0 ]
      _ | even stoneLength ->
        fromMaybe' (\_ -> unsafePartial $ crashWith "???") do
          let { before, after } = String.splitAt (stoneLength / 2) $ show stone
          stone1 <- hush $ runParser before number
          stone2 <- hush $ runParser after number
          Just [ stone1, stone2 ]
      _ -> [ stone * 2024.0 ]

parser = number `sepBy` space <#> fromFoldable
