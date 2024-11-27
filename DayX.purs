module DayDAY_NUM where

import Prelude

import Effect (Effect)
import Main (run)
import Partial (crashWith)
import Data.Maybe (Maybe(..))
import Debug (spyWith)

main ∷ Effect Unit
main = run DAY_NUM Nothing parser calc

calc lines =
  let
    _ = spyWith "" show lines
  in
    crashWith "calc not implemented"

parser = crashWith "parser not implemented"