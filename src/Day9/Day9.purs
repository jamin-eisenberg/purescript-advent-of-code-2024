module Day9 where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.ST (while)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (modify, new, read, write)
import Data.Array (catMaybes, concat, cons, filter, findIndex, findLastIndex, length, mapWithIndex, partition, replicate, zip)
import Data.Array.ST (STArray, freeze, thaw)
import Data.Array.ST as STArray
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Int (even, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), curry, fst, snd, uncurry)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing.Combinators.Array (many)
import Parsing.String (rest, takeN)
import Parsing.String.Basic (digit, intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 9 Nothing parser (Right <<< calc)

calc str =
  let
    digits = str # toCharArray <#> toCharCode <#> (_ - toCharCode '0')
    blocks = getBlocks digits # concat
    condensed = condense blocks
  in
    ( ( condensed
          <#>
            ( case _ of
                Free -> Nothing
                File id -> Just id
            )
      )
        # catMaybes
        <#> toNumber
    )
      `flip mapWithIndex` (\i n -> toNumber i * n)
      # sum

getBlocks =
  mapWithIndex
    ( \i blockSize ->
        if even i then
          replicate blockSize $ File $ i / 2
        else replicate blockSize Free
    )

condense :: Array Block -> Array Block
condense immutableBlocks =
  ST.run
    do
      blocks <- thaw immutableBlocks
      pointerStart <- new 0
      pointerEnd <- new $ length immutableBlocks - 1
      while ((<) <$> read pointerStart <*> read pointerEnd) do
        currPointerStart <- read pointerStart
        currPointerEnd <- read pointerEnd
        currBlock <- STArray.peek currPointerStart blocks
        if currBlock == Just Free then do
          swap currPointerStart currPointerEnd blocks
          currBlocks <- freeze blocks
          let lastNonFree = findLastIndex (notEq Free) currBlocks # fromMaybe (-1)
          void $ write lastNonFree pointerEnd
        else
          pure unit
        modify (_ + 1) pointerStart
      freeze blocks

swap :: forall h a. Int -> Int -> STArray h a -> ST.ST h Unit
swap i1 i2 arr =
  do
    e1' <- STArray.peek i1 arr
    e2' <- STArray.peek i2 arr
    case e1', e2' of
      Just e1, Just e2 ->
        do
          _ <- STArray.poke i1 e2 arr
          void $ STArray.poke i2 e1 arr
      _, _ -> pure unit

instance Show Block where
  show = case _ of
    Free -> "."
    File id -> show id

type Id = Int
data Block = File Id | Free

derive instance Eq Block

parser = rest