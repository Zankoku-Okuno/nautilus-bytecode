{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Nautilus.Bytecode.Machine (
    -- * Interpreter Monad
      NBCI
    , runNBCI
    -- * Primitive Actions
    , peekIP
    , moveIP
    -- * Inspection
    , infoIP
    -- ** Machine Dump
    , Machine(..)
    , Frame(..)
    ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Language.Nautilus.Bytecode.Abstract


data Frame = Frame {
      frameinfo_name :: BlockName
    , frame_previnstrs :: [Instr]
    , frame_nextinstrs :: [Instr]
    , frame_stack :: [Data]
    , frame_argsprep :: [Data]
    , frame_retsprep :: [Data]
    , frame_last :: Maybe Frame
}

data Machine = Machine {
      machine_frames :: Frame
    , machine_text :: Map BlockName [Instr]
}

newtype NBCI a = NBCI { unNBCI :: StateT Machine IO a }
    deriving (Functor, Applicative, Monad)

runNBCI :: (BlockName, Map BlockName [Instr]) -> NBCI () -> IO Machine
runNBCI (entry, text) action = flip execStateT machine0 $ unNBCI action
    where
    machine0 = Machine {
      machine_frames = frame0
    , machine_text = text
    }
    frame0 = Frame {
      frameinfo_name = entry
    , frame_previnstrs = []
    , frame_nextinstrs = fromJust $ Map.lookup entry text
    , frame_stack = []
    , frame_argsprep = []
    , frame_retsprep = []
    , frame_last = Nothing
    }


peekIP :: NBCI Instr
peekIP = NBCI $ gets (head . frame_nextinstrs . machine_frames)

moveIP :: Offset -> NBCI ()
moveIP offset = NBCI $ do
    fs <- gets machine_frames
    let fs' = go offset fs
    modify $ \s -> s { machine_frames = fs' }
    where
    go i fs | i < 0 =
                let prev = frame_previnstrs fs
                    (prev', move) = splitAt (length prev + i) prev
                    next' = move ++ frame_nextinstrs fs
                in fs { frame_previnstrs = prev'
                      , frame_nextinstrs = next' }
            | i == 0 = fs
            | i > 0 = 
                let (move, next') = splitAt i (frame_nextinstrs fs)
                    prev' = frame_previnstrs fs ++ move
                in fs { frame_previnstrs = prev'
                      , frame_nextinstrs = next' }

-- pushData n
-- popData n
-- peekData ix

-- prepArg
-- flushArgsPrep
-- prepRet
-- flushRetsPrep

-- pushFrame
-- popFrame


infoIP :: NBCI (BlockName, Int)
infoIP = NBCI $ do
    fs <- gets machine_frames
    let offset = length $ frame_previnstrs fs
        name = frameinfo_name fs
    return (name, offset)