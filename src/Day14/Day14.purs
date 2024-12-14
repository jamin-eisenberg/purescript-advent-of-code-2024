module Day14 where

import Prelude

import Effect (Effect)
import Main (run)
import Partial (crashWith)
import Data.Maybe (Maybe(..))
import Debug (spyWith)

main ∷ Effect Unit
main = run 14 Nothing parser calc

calc lines =
  let
    _ = spyWith "" show lines
  in
    crashWith "calc not implemented"

parser = crashWith "parser not implemented"