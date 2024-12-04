module Day4 where

import Prelude

import Data.Array (catMaybes, concat, filter, fromFoldable, index, length, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (iterateN)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, char, satisfy)
import Partial (crashWith)

main ∷ Effect Unit
main = run 4 Nothing parser (Right <<< calc)

type Point = { x :: Int, y :: Int }

calc :: Array (Array Char) -> Int
calc grid =
  let
    xsCoords =
      mapWithIndex
        ( \y row -> mapWithIndex
            (\x element -> if element == 'X' then Just { x, y } else Nothing)
            row
        )
        grid
        <#> catMaybes
        # concat

    possibleXmass = xsCoords <#> directionalLines # concat
  in
    filter (isLineXmas grid) possibleXmass
      # length

get2d :: forall a. Point -> Array (Array a) -> Maybe a
get2d { x, y } grid = do
  row <- grid !! y
  element <- row !! x
  Just element

directionalPoints ∷ Array Point
directionalPoints =
  [ { x: 0, y: -1 }
  , { x: 1, y: -1 }
  , { x: 1, y: 0 }
  , { x: 1, y: 1 }
  , { x: 0, y: 1 }
  , { x: -1, y: 1 }
  , { x: -1, y: 0 }
  , { x: -1, y: -1 }
  ]

directionalLines ∷ Point -> Array (Array Point)
directionalLines p = directionalPoints
  <#>
    ( \directionalPoint ->
        (iterateN 4 (_ + directionalPoint) p)
    )

isLineXmas :: Array (Array Char) -> Array Point -> Boolean
isLineXmas grid points = [ 'X', 'M', 'A', 'S' ] == (points <#> flip get2d grid # catMaybes)

parser :: Parser String (Array (Array Char))
parser = (many $ satisfy (notEq '\n')) `sepBy` char '\n' <#> fromFoldable