module Day14 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (product)
import Data.Function (applyN)
import Data.List (List, filter, length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing.Combinators (sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 14 Nothing parser (Right <<< calc)

width = 101
height = 103

calc ∷ List Robot → Int
calc robots =
  let
    finalRobots = robots <#> applyN move 100 <#> _.pos
    quad (Tuple compX compY) = finalRobots # filter (\{ x, y } -> (compX x (width / 2)) && compY y (height / 2))
    quadrantsComps = [ Tuple (<) (<), Tuple (<) (>), Tuple (>) (<), Tuple (>) (>) ]
  in
    quadrantsComps <#> quad <#> (spyWith "" show) <#> length # product

type Robot = { pos :: { x :: Int, y :: Int }, vel :: { x :: Int, y :: Int } }

move :: Robot -> Robot
move robot@{ pos, vel } = robot
  { pos =
      { x: (pos.x + vel.x) `mod` width, y: (pos.y + vel.y) `mod` height }
  }

parser = robot `sepBy` char '\n'

robot = do
  _ <- string "p="
  px <- intDecimal
  _ <- char ','
  py <- intDecimal
  _ <- string " v="
  vx <- intDecimal
  _ <- char ','
  vy <- intDecimal
  pure { pos: { x: px, y: py }, vel: { x: vx, y: vy } }