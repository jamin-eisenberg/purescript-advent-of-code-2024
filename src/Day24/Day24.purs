module Day24 where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable, mapWithIndex, sort)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..), fst)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Options.Applicative.Internal.Utils (startsWith)
import Parsing (Parser)
import Parsing.Combinators (choice, sepBy, try)
import Parsing.String (anyTill, char, string, takeN)
import Parsing.String.Basic (skipSpaces)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 24 (Just $ replaceAll (Pattern "\n\n") (Replacement " ")) parser (Right <<< calc)

calc connections =
  let
    _ = spyWith "" show connections
    zKeys = Map.keys connections
      # Set.filter (startsWith (Pattern "z"))
      # fromFoldable
      # sort
  in
    zKeys
      <#> flip eval connections
      <#> (\b -> if b then 1 else 0)
      # foldrWithIndex (\i n total -> total + (toNumber n) * pow 2.0 (toNumber i)) 0.0

eval :: String -> Map String Input -> Boolean
eval wire connections =
  case Map.lookup wire connections of
    Nothing -> unsafePartial $ crashWith $ "no such wire" <> wire
    Just (Init val) -> val
    Just (Gate { in1, in2, gate }) ->
      let
        in1Val = eval in1 connections
        in2Val = eval in2 connections
        op = opFromGate gate
      in
        in1Val `op` in2Val

opFromGate ∷ Gate → Boolean → Boolean → Boolean
opFromGate =
  case _ of
    Xor -> (/=)
    And -> (&&)
    Or -> (||)

data Gate = Xor | And | Or

derive instance Generic Gate _
instance Show Gate where
  show = genericShow

data Input = Gate { in1 :: String, in2 :: String, gate :: Gate } | Init Boolean

derive instance Generic Input _
instance Show Input where
  show = genericShow

parser ∷ Parser String (Map String Input)
parser = do
  initialValues <- initialValuesParser
  _ <- string " "
  connections <- connectionsParser
  pure $ Map.union initialValues connections

initialValuesParser :: Parser String (Map String Input)
initialValuesParser = try initialValueParser `sepBy` char '\n' <#> Map.fromFoldable

initialValueParser :: Parser String (Tuple String Input)
initialValueParser = do
  wire <- anyTill (string ": " <|> string "\n") <#> fst
  value <- choice [ char '1' $> true, char '0' $> false ]
  pure $ Tuple wire $ Init value

connectionsParser :: Parser String (Map String Input)
connectionsParser = connectionParser `sepBy` char '\n' <#> Map.fromFoldable

connectionParser :: Parser String (Tuple String Input)
connectionParser = do
  in1 <- takeN 3
  skipSpaces
  gate <- choice [ string "XOR" $> Xor, string "AND" $> And, string "OR" $> Or ]
  skipSpaces
  in2 <- takeN 3
  _ <- string " -> "
  out <- takeN 3
  pure $ Tuple out $ Gate { in1, in2, gate }