module Day9 where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.ST (while)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (new, read)
import Data.Array (concat, cons, filter, findIndex, length, mapWithIndex, partition, zip)
import Data.Array.ST (thaw)
import Data.Array.ST as STArray
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
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
    blocks = spyWith "" show $ getBlocks digits
  in
    1

getBlocks =
  mapWithIndex (\i blockSize -> if even i then File i blockSize else Free blockSize)

condense :: Array Block -> Array Block
condense blocks =
  []

-- ST.run
--   do
--     blocks <- thaw immutableBlocks
--     pointerStart <- new 0
--     pointerEnd <- new $ length immutableBlocks
--     while ((>) <$> read pointerStart <*> read pointerEnd) do
--       currPointerStart <- read pointerStart
--       currPointerEnd <- read pointerEnd
--     pure []

type Id = Int
data Block = File Id Int | Free Int

derive instance Generic Block _
instance Show Block where
  show = genericShow

parser = rest