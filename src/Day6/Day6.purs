module Day6 where

import Prelude

import Control.Monad.ST (while)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (modify, new, read, write)
import Data.Array (catMaybes, concat, elem, foldl, fromFoldable, head, length, mapWithIndex, partition)
import Data.Either (note)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.FunctorWithIndex as FunctorWithIndex
import Data.Generic.Rep (class Generic)
import Data.HeytingAlgebra (ff)
import Data.List (List(..), filter, nub, singleton, take, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing.Combinators (choice, sepBy)
import Parsing.Combinators.Array (many)
import Parsing.String (char)
import Partial (crashWith)

main ∷ Effect Unit
main = run 6 Nothing parser (note "no start found" <<< calc)

calc grid =
  let
    _ = spyWith "" show grid
    nonEmptyCoords =
      mapWithIndex
        ( \y row -> mapWithIndex
            (\x element -> if element /= Empty then Just { x, y, element } else Nothing)
            row
        )
        grid
        # concat
        # catMaybes
    { yes: startAllEls, no: obstacleEls } = partition (_.element >>> eq Start) nonEmptyCoords
    startAllCoords = startAllEls <#> \e -> { x: e.x, y: e.y }
    obstacleCoords = obstacleEls <#> \e -> { x: e.x, y: e.y }
  in
    do
      startCoords <- head startAllCoords
      width <- head grid <#> length
      let height = length grid
      let
        visited =
          visitedPositions
            { startPos: startCoords, startDir: Up }
            obstacleCoords
            width
            height
            Nil
            # _.visited
      pure $
        visited
          `flip FunctorWithIndex.mapWithIndex`
            ( \i (Tuple pos dir) ->
                visitedPositions { startDir: rotateRight dir, startPos: pos } obstacleCoords width height (take i visited)
            )
          # List.filter _.looped
          <#> spyWith "" show
          # List.length

type Pos = { x :: Int, y :: Int }

visitedPositions ∷ { startDir ∷ Direction, startPos ∷ Pos } → Array Pos → Int → Int -> List (Tuple Pos Direction) → { visited :: List (Tuple Pos Direction), looped :: Boolean }
visitedPositions { startPos, startDir } obstacleCoords width height startVisited = ST.run do
  pos <- new startPos
  dir <- new startDir
  visited <- new startVisited
  looped <- new false
  while
    ( do
        currentPos <- read pos
        currentLooped <- read looped
        pure $ isInBounds width height currentPos && not currentLooped
    )
    do
      currentPos <- read pos
      currentDir <- read dir
      currentVisited <- read visited
      when (Tuple currentPos currentDir `List.elem` currentVisited) do
        void $ write true looped
      _ <- modify (Tuple currentPos currentDir : _) visited
      let { newPos, newDir } = move currentPos currentDir obstacleCoords
      _ <- write newPos pos
      write newDir dir
  currentVisited <- read visited
  currentLooped <- read looped
  pure { visited: currentVisited, looped: currentLooped }

move pos dir obstacleCoords =
  let
    newPotentialPos = pos + directionVec dir
  in
    if newPotentialPos `elem` obstacleCoords then
      let
        newDir = rotateRight dir
      in
        { newPos: pos + directionVec newDir, newDir }
    else { newPos: newPotentialPos, newDir: dir }

isInBounds width height { x, y } = between 0 (width - 1) x && between 0 (height - 1) y

data Direction = Up | Right | Down | Left

derive instance Eq Direction
derive instance Generic Direction _
instance Show Direction where
  show = genericShow

rotateRight = case _ of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

directionVec = case _ of
  Up -> { x: 0, y: -1 }
  Right -> { x: 1, y: 0 }
  Down -> { x: 0, y: 1 }
  Left -> { x: -1, y: 0 }

parser = many elementParser `sepBy` char '\n' <#> fromFoldable

data Element = Obstacle | Empty | Start

derive instance Eq Element

derive instance Generic Element _
instance Show Element where
  show = genericShow

elementParser = choice [ char '#' $> Obstacle, char '.' $> Empty, char '^' $> Start ]