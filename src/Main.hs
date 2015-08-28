module Main where

import System.Exit

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Language.Nautilus.Bytecode.Machine (runNBCI)
import Language.Nautilus.Bytecode.Abstract
import Language.Nautilus.Bytecode.Interpreter

main :: IO ()
main = do
    (exitcode, dump) <- runNBCI ("foo", text) debugCycles
    putStrLn $ "[nbci] Exiting with code " ++ show exitcode ++ "."
    print dump
    exitWith $ if exitcode == 0 then ExitSuccess else ExitFailure (fromIntegral exitcode)


text = Map.fromList [
      ("foo", [ Push (U8 3)
              , JumpRel EqZ 1
              , Nop
              , Push (U8 0)
              , Halt
              ])
    ]