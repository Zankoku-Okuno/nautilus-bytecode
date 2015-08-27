module Language.Nautilus.Bytecode.Abstract where

import Data.IORef

{-

jump <condition> <offset> -- within block, relative offset by number of instructions
jump <condition> <blockname>
call <block name | pop> -- returns to the instr after call instr
tailcall <block name> then <block name> -- called routine returns to the then block
pushblock <blckname> -- push the blockname on the stack
computed (tail)call -- pop 1 or two blocknames off stack, and call or tailcall

name mangling: when a name is not exported by an object file, its BlockName is mangled to start with a dot, and end with a '$' and the SHA256 hash of the (pre-mangling) object file text+data

-}


type BlockName = String
type Offset = Int

data Type =
      I8_t | I16_t | I32_t | I64_t | I128_t
    | U8_t | U16_t | U32_t | U64_t | U128_t
    | F32_t | F64_t | F128_t
    | TextAddr_t | DataAddr_t | PtrOffset_t
    deriving (Show)

data Data =
      TextAddr BlockName
    | DataAddr (IORef [Data])
    | PtrOffset Offset
    | Void
    --deriving (Show)

data Instr =
      Call BlockName
    | TailCall BlockName BlockName
    | ComputedCall
    | ComputedTailCall
    | JumpRel Condition Offset
    | JumpAbs Condition BlockName
    deriving (Show)

data Condition =
      Always
    | EqZ | NZ
    | Eq | NEq
    | Lt | LtE
    | Gt | GtE
    -- TODO on condition flags
    deriving (Show)

--TODO represent a library