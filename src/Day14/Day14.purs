module Day14 where

import Prelude

import Data.Array (range)
import Data.Either (Either(..))
import Data.Foldable (elem, product)
import Data.Function (applyN)
import Data.List (List, filter, length)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
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

-- width = 11
-- height = 7

calc ∷ List Robot → MyString
calc robots =
  let
    { log } = { robots, i: 0, log: "" } # applyN moveAndShowAll 10000
  in
    MyString log

newtype MyString = MyString String

instance Show MyString where
  show (MyString s) = s

moveAndShowAll ∷ { robots :: List Robot, i :: Int, log :: String } -> { robots :: List Robot, i :: Int, log :: String }
moveAndShowAll { robots, i, log } =
  let
    moved = robots <#> move
  in
    { robots: moved, i: i + 1, log: log <> ("\n\n" <> show i <> "\n") <> (((_ <#> _.pos) >>> Set.fromFoldable >>> showGrid) moved) }

showGrid :: Set { x :: Int, y :: Int } -> String
showGrid poss =
  let
    charGrid = range 0 (height - 1) <#>
      \y -> range 0 (width - 1) <#>
        \x -> if { x, y } `Set.member` poss then '0' else '.'
  in
    charGrid
      <#> fromCharArray
      # joinWith "\n"

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