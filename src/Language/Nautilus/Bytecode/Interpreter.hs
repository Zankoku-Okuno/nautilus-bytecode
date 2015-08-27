module Language.Nautilus.Bytecode.Interpreter where

import Control.Applicative
import Control.Monad

import Language.Nautilus.Bytecode.Abstract
import Language.Nautilus.Bytecode.Machine


fetch :: NBCI Instr
fetch = do
    instr <- peekIP
    moveIP 1
    return instr

execute :: Instr -> NBCI ()
execute it = error $ "unimplemented: " ++ show it

cycles :: NBCI ()
cycles = fetch >>= execute >> cycles