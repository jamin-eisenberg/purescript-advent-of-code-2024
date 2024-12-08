module Day8 where

import Prelude

import Control.Alternative (guard)
import Data.Array (catMaybes, concat, filter, fromFoldable, groupBy, length, mapWithIndex, notElem, nub, sortBy, sortWith)
import Data.Array.NonEmpty as NEA
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Function (on)
import Data.Map (values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (abs)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (char, satisfy)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 8 Nothing parser (Right <<< calc)

type Pos = { x :: Int, y :: Int }

calc ∷ Array (Array Char) → Int
calc grid =
  let
    height = length grid
    width = length $ unsafePartial $ head grid
    coordMap = spyWith "" show $
      mapWithIndex
        ( \y row -> mapWithIndex
            (\x element -> if element /= '.' then Just { x, y, element } else Nothing)
            row
        )
        grid
        # concat
        # catMaybes
        # sortWith _.element
        # groupBy (eq `on` _.element)
        <#> (\group -> Tuple (group # NEA.head # _.element) (group <#> \node -> { x: node.x, y: node.y }))
        # Map.fromFoldable
  in
    coordMap
      <#> (fromFoldable >>> validAntinodesForGroup width height)
      # values >>> fromFoldable
      # concat
      # nub
      # length

validAntinodesForGroup :: Int -> Int -> Array Pos -> Array Pos
validAntinodesForGroup width height poss =
  filter (inBounds width height)
    ( concat do
        p1 <- poss
        p2 <- poss
        guard $ p1 /= p2
        pure $ antinodes p1 p2
    )

inBounds width height { x, y } = between 0 (width - 1) x && between 0 (height - 1) y

antinodes :: Pos -> Pos -> Array Pos
antinodes p1 p2 =
  let
    diffVec = abs (p1 - p2)
  in
    [ p1 - diffVec, p1 + diffVec, p2 - diffVec, p2 + diffVec ]

parser :: Parser String (Array (Array Char))
parser = (many $ satisfy (notEq '\n')) `sepBy` char '\n' <#> fromFoldable