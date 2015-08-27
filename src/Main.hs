module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Language.Nautilus.Bytecode.Machine (runNBCI)
import Language.Nautilus.Bytecode.Abstract
import Language.Nautilus.Bytecode.Interpreter

main :: IO ()
main = void $ runNBCI ("foo", text) cycles

text = Map.fromList [
      ("foo", [Call "foo"])
    ]