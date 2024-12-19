module Day17 where

import Prelude

import Control.Monad.ST (while)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (modify, new, read, write)
import Data.Array (findIndex, foldl, fromFoldable, index, length, range)
import Data.Array.ST (STArray, freeze)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (pow)
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (liftMaybe)
import Parsing.Combinators (sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

main ∷ Effect Unit
main = run 17 Nothing parser (Right <<< calc)

calc { initB, initC, instructions } =
  let
    _ = spyWith "" show instructions
    rawInstructions = [ 2, 4, 1, 2, 7, 5, 1, 3, 4, 3, 5, 5, 0, 3, 3, 0 ]
  in
    findIndex
      ( \initA -> rawInstructions ==
          spyWith "" show (execute { initA, initB, initC } instructions)
      ) $ range 0 100000100

execute :: { initA :: Int, initB :: Int, initC :: Int } -> Array Instruction -> Array Int
execute { initA, initB, initC } instructions =
  ST.run
    do
      ip <- new 0
      a <- new initA
      b <- new initB
      c <- new initC
      output <- STArray.new
      while (read ip <#> (_ < length instructions)) do
        currIp <- read ip
        currA <- read a
        currB <- read b
        currC <- read c
        let Instruction opCode operand = unsafePartial $ fromJust $ index instructions currIp
        let literalOp = operand
        let comboOp = unsafePartial $ fromJust $ index [ 0, 1, 2, 3, currA, currB, currC, -1 ] operand
        case opCode of
          Adv -> write_ (currA / pow 2 comboOp) a
          Bxl -> write_ (xor currB literalOp) b
          Bst -> write_ (comboOp `mod` 8) b
          Jnz -> if currA == 0 then pure unit else write_ (literalOp - 1) ip
          Bxc -> write_ (xor currB currC) b
          Out -> void $ STArray.push (comboOp `mod` 8) output
          Bdv -> write_ (currA / pow 2 comboOp) b
          Cdv -> write_ (currA / pow 2 comboOp) c
        void $ modify (_ + 1) ip
      freeze output

write_ val ref = do
  _ <- write val ref
  pure unit

data OpCode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

derive instance Generic OpCode _
instance Show OpCode where
  show = genericShow

toOpCode :: Int -> Maybe OpCode
toOpCode = index [ Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv ]

data Instruction = Instruction OpCode Int

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

parser = do
  _ <- string "Register A: "
  initA <- intDecimal
  _ <- string "\nRegister B: "
  initB <- intDecimal
  _ <- string "\nRegister C: "
  initC <- intDecimal
  _ <- string "\n\nProgram: "
  instructions <- instructionParser `sepBy` char ',' <#> fromFoldable
  pure { initA, initB, initC, instructions }

instructionParser = do
  opCodeRaw <- intDecimal
  _ <- char ','
  operand <- intDecimal
  opCode <- liftMaybe (\_ -> "expected an op code 0-7") $ toOpCode opCodeRaw
  pure $ Instruction opCode operand

-- Decode.field "input" Decode.string
--  |> Decode \input -> Decode.field "time" Iso.decoder 
--  |> Decode \time -> ...
--- |> Decode \day -> Bullet input time ... day

-- Decode.map8 Bullet "time" "day"

-- do laundry: 1
-- do homework: 
-- go to work: 
-- philosophy: 0

-- 0: think
-- 1: household