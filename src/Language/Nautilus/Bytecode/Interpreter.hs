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
execute (Peek ix) = peekData (fromIntegral ix) >>= pushData
execute (JumpRel condition offset) = do
    doJump <- cond condition
    when doJump $ moveIP offset
execute it = error $ "unimplemented: " ++ show it

cond :: Condition -> NBCI Bool
cond Always = return True
cond EqZ = do
    [test] <- popData 1
    return $ fromNBC test == (0 :: Integer)
cond it = error $ "unimplemented condition: " ++ show it


cycles :: NBCI ()
cycles = fetch >>= execute >> cycles

debugCycles :: NBCI ()
debugCycles = do
    instr <- fetch
    system $ print instr
    execute instr
    debugCycles
