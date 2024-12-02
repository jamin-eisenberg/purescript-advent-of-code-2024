module Day2 where

import Prelude

import Data.Either (Either(..))
import Data.Function (on)
import Data.List (List(..), all, drop, filter, foldr, length, sort, sortBy, take, (:))
import Data.List.NonEmpty (fromList)
import Data.List.Partial (tail)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (abs)
import Data.Ordering (invert)
import Data.Semigroup.Foldable (foldr1)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, space)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 2 Nothing parser (Right <<< calc)

-- calc :: List (List Int) -> Int
calc rows =
  filter safe rows
    # length

slidingWindow :: forall a. Int -> List a -> List (NonEmptyList a)
slidingWindow windowSize xs =
  if length xs >= windowSize then
    (unsafePartial $ fromJust $ fromList $ take windowSize xs)
      : slidingWindow windowSize (unsafePartial $ tail xs)
  else Nil

safe :: List Int -> Boolean
safe =
  let
    increasing xs = sort xs == xs
    decreasing xs = sortBy (\x y -> compare y x) xs == xs
    sufficientlyGradual xs =
      all
        ( \windows -> foldr1 (-) windows
            # abs
            # between 1 3
        ) $ slidingWindow 2 xs
  in
    (increasing || decreasing) && sufficientlyGradual

parser :: Parser String (List (List Int))
parser = (intDecimal `sepBy` string " ") `sepBy` string "\n"