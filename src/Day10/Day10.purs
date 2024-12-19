module Day10 where

import Prelude

import Data.Array (catMaybes, concat, filter, fromFoldable, index, length, mapWithIndex, nub)
import Data.Array.Partial (head)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing.Combinators (sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (char)
import Parsing.String.Basic (digit)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 10 Nothing parser (Right <<< calc)

calc rawGrid =
  let
    grid = rawGrid <#> (_ <#> toCharCode >>> (_ - toCharCode '0'))
    height = length grid
    width = length $ unsafePartial $ head grid
    neighborGraph = spyWith "" show $
      mapWithIndex
        ( \y row ->
            mapWithIndex (\x el -> Tuple { x, y } $ Tuple (neighbors { x, y } grid) el) row
        )
        grid
        # concat
        # Map.fromFoldable
    starts =
      mapWithIndex
        ( \y row ->
            mapWithIndex (\x el -> if el == 0 then Just { x, y } else Nothing) row
        )
        grid
        # concat
        # catMaybes
  in
    starts
      <#> flip fullyTraversableCount neighborGraph
      <#> nub
      <#> length
      # sum

fullyTraversableCount :: Pos -> Map Pos (Tuple (Array Pos) Int) -> Array Pos
fullyTraversableCount start neighborGraph =
  case Map.lookup (spyWith "" show start) neighborGraph of
    Nothing -> []
    Just (Tuple paths val) ->
      if val == 9 then [ start ]
      else
        paths <#> flip fullyTraversableCount neighborGraph # concat

type Pos = { x :: Int, y :: Int }

index2d { x, y } grid = index grid y >>= flip index x

neighbors :: Pos -> Array (Array Int) -> Array Pos
neighbors { x, y } grid =
  let
    rawNeighborPoss = [ { x: x - 1, y }, { x: x + 1, y }, { x, y: y - 1 }, { x, y: y + 1 } ]
    currHeight = unsafePartial $ fromJust $ index2d { x, y } grid
  in
    rawNeighborPoss
      # filter (\pos -> index2d pos grid == Just (currHeight + 1))

inBounds width height { x, y } = between 0 (width - 1) x && between 0 (height - 1) y

parser = (many digit) `sepBy` char '\n' <#> fromFoldable

