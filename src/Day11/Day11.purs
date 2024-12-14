module Day11 where

import Prelude

import Data.Array (concatMap, fromFoldable, last, length)
import Data.Either (hush, note)
import Data.Function.Memoize (memoize)
import Data.Int (even)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String as String
import Data.Unfoldable1 (iterateN)
import Debug (spyWith)
import Effect (Effect)
import JS.BigInt (BigInt, fromInt, fromNumber)
import Main (run)
import Parsing (runParser)
import Parsing.Combinators (sepBy)
import Parsing.String.Basic (intDecimal, number, space)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

-- main ∷ Effect Unit
-- main = run 11 Nothing parser (note "empty stones" <<< calc)

calc stones =
  iterateN 76 (concatMap $ blink) stones
    # last
    <#> length

blink :: BigInt -> Array BigInt
blink stone =
  let
    stoneLength = (String.length $ show stone) - 2
  in
    if stone == fromInt 0 then [ fromInt 1 ]
    else if even stoneLength then
      fromMaybe' (\_ -> unsafePartial $ crashWith "???") do
        let { before, after } = String.splitAt (stoneLength / 2) $ show stone
        stone1 <- hush (runParser before number) >>= fromNumber
        stone2 <- hush (runParser after number) >>= fromNumber
        Just [ stone1, stone2 ]
    else [ stone * fromInt 2024 ]

parser = number `sepBy` space <#> fromFoldable
