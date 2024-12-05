module Day5 where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (all, elem, sum)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), catMaybes, filter, findIndex, groupBy, length, sort, sortBy, zip, (!!))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), curry, fst, snd)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing.Combinators (manyTill, manyTill_, optional, sepBy, try)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 5 Nothing parser (Either.note "missing middle" <<< calc)

calc { constraints, updates } =
  let
    constraintsMap = compileConstraints constraints
  in
    filter (not $ obeysConstraints constraintsMap) updates
      <#> reorder constraintsMap
      <#> getMiddle
      # sum

reorder :: Map Int (NonEmptyList Int) -> List Int -> List Int
reorder constraintsMap update =
  let
    constraintListLength = elementConstraints constraintsMap update >>> length
  in
    sortBy (compare `on` constraintListLength) update

obeysConstraints :: Map Int (NonEmptyList Int) -> List Int -> Boolean
obeysConstraints constraintsMap update =
  all
    ( \element -> elementObeysConstraints
        (elementConstraints constraintsMap update element)
        update
        element
    )
    update

elementObeysConstraints :: List Int -> List Int -> Int -> Boolean
elementObeysConstraints afterElement update element =
  let
    elementIndex = findIndex (eq element) update
  in
    all (\after -> findIndex (eq after) update > elementIndex) afterElement

elementConstraints :: Map Int (NonEmptyList Int) -> List Int -> Int -> List Int
elementConstraints constraintsMap relevant key =
  case Map.lookup key constraintsMap of
    Nothing -> Nil
    Just constraints -> NE.filter (flip elem relevant) constraints

getMiddle :: forall a. List a -> Maybe a
getMiddle xs = xs !! (length xs / 2)

compileConstraints constraintList =
  constraintList
    # sort
    # groupBy (eq `on` fst)
    <#> (\group -> Tuple (NE.head group # fst) (group <#> snd))
    # Map.fromFoldable

parser = do
  constraints <- constraintsParser
  updates <- updatesParser
  pure { constraints, updates }

constraintsParser = manyTill_ constraintParser (string "\n\n") <#> fst

constraintParser = do
  optional $ char '\n'
  n1 <- intDecimal
  _ <- char '|'
  n2 <- intDecimal
  pure $ Tuple n1 n2

updatesParser = (intDecimal `sepBy` char ',') `sepBy` char '\n'