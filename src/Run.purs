module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Options.Applicative as Options
import Parsing (Parser, runParser) as Parsing
import Parsing.String (parseErrorHuman) as Parsing

data Input = Sample | Real

inputFilename :: Input -> String
inputFilename =
  case _ of
    Sample -> "sample.txt"
    Real -> "real.txt"

newtype Args = Args { input :: Input }

sample :: Options.Parser Args
sample = ado
  input <-
    Options.flag' Sample (Options.long "sample") <|>
      Options.flag' Real (Options.long "real")

  in Args { input }

run :: forall a b. Show b => Int -> Maybe (String -> String) -> (Parsing.Parser String a) -> (a -> Either String b) -> Effect Unit
run day preprocessor parser calculator = do
  Args { input } <- Options.execParser (Options.info sample Options.fullDesc)
  contents <- readTextFile UTF8 ("src" <> "/Day" <> show day <> "/" <> inputFilename input)
  let
    preprocessed = case preprocessor of
      Nothing -> contents
      Just f -> f contents
  log
    case Parsing.runParser preprocessed parser of
      Left parseError ->
        Parsing.parseErrorHuman contents 100 parseError
          # String.joinWith "\n"
      Right parsed ->
        case calculator parsed of
          Left calculationError -> "An error occurred:\n\t" <> show calculationError
          Right result -> show result

