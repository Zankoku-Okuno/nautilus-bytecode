module Language.Nautilus.Bytecode.Interpreter where

import Control.Applicative
import Control.Monad

import Data.Word

import Language.Nautilus.Bytecode.Abstract
import Language.Nautilus.Bytecode.FromNBC
import Language.Nautilus.Bytecode.Machine


fetch :: NBCI Instr
fetch = do
    instr <- peekIP
    moveIP 1
    return instr

execute :: Instr -> NBCI ()
execute Nop = return ()
execute Halt = halt =<< fromNBC1 <$> popData 1
execute (Push x) = pushData x
execute it = error $ "unimplemented: " ++ show it

cycles :: NBCI ()
cycles = fetch >>= execute >> cycles

debugCycles :: NBCI ()
debugCycles = do
    instr <- fetch
    system $ print instr
    execute instr
    debugCycles
