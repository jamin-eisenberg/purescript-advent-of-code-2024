module Day7 where

import Prelude

import Data.Array (fromFoldable, uncons)
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Foldable (any, sum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), filter, length, reverse, (:))
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (runParser)
import Parsing.Combinators (sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal, number)
import Partial (crash, crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 7 Nothing parser (Right <<< calc)

calc lines =
  filter (spyWith "" show >>> solvable) lines
    <#> _.result
    # sum

solvable :: { result :: Number, operands :: Array Number } -> Boolean
solvable { result, operands } =
  let
    eval = flip interleaveApply (Array.reverse operands)
  in
    Array.any (eval >>> eq result) (operatorPermutations $ Array.length operands - 1)

interleaveApply :: Array Operator -> Array Number -> Number
interleaveApply operators operands =
  case uncons operators, uncons operands of
    Nothing, Just { head: operand, tail: [] } -> operand
    Just { head: operator, tail: operatorRest }, Just { head: operand, tail: operandRest } ->
      (applyOp operator) operand (interleaveApply operatorRest operandRest)
    _, _ -> unsafePartial $ crashWith "malformed operator/rand list"

operatorPermutations :: Int -> Array (Array Operator)
operatorPermutations count = sequence (replicate count [ Add, Mul, Concat ])

data Operator = Add | Mul | Concat

derive instance Generic Operator _
instance Show Operator where
  show = genericShow

applyOp = case _ of
  Add -> (+)
  Mul -> (*)
  Concat -> \x y -> append (toString y) (toString x) # flip runParser number # case _ of
    Left _ -> unsafePartial $ crashWith "badnum"
    Right n -> n

parser = equationParser `sepBy` char '\n'

equationParser = do
  result <- number
  _ <- string ": "
  operands <- number `sepBy` char ' ' <#> fromFoldable
  pure { result, operands }