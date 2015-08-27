{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Nautilus.Bytecode.Machine (
    -- * Interpreter Monad
      NBCI
    , runNBCI
    -- * Primitive Actions
    , system
    , peekIP
    , moveIP
    , halt
    , pushData
    , popData
    -- * Inspection
    , infoIP
    -- ** Machine Dump
    , Machine(..)
    , Frame(..)
    ) where

import Data.Word
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

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
    deriving (Show)

data Machine = Machine {
      machine_frames :: Frame
    , machine_text :: Map BlockName [Instr]
}
    deriving (Show)

newtype NBCI a = NBCI { unNBCI :: ExceptT Word8 (StateT Machine IO) a }
    deriving (Functor, Applicative, Monad)

runNBCI :: (BlockName, Map BlockName [Instr]) -> NBCI () -> IO (Word8, Machine)
runNBCI (entry, text) action = flip runStateT machine0 $ evalExceptT $ unNBCI action
    where
    evalExceptT action = do
        result <- runExceptT action
        return $ case result of
            Left e -> e
            Right _ -> error "improper halt"
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

system :: IO a -> NBCI a
system = NBCI . liftIO

halt :: Word8 -> NBCI ()
halt exitcode = NBCI $ throwError exitcode


peekIP :: NBCI Instr
peekIP = NBCI $ gets (head . frame_nextinstrs . machine_frames)

moveIP :: Offset -> NBCI ()
moveIP offset = NBCI $ do
    frame <- gets machine_frames
    let frame' = go offset frame
    modify $ \m -> m { machine_frames = frame' }
    where
    go i frame | i < 0 =
                let prev = frame_previnstrs frame
                    (prev', move) = splitAt (length prev + i) prev
                    next' = move ++ frame_nextinstrs frame
                in frame { frame_previnstrs = prev'
                      , frame_nextinstrs = next' }
            | i == 0 = frame
            | i > 0 = 
                let (move, next') = splitAt i (frame_nextinstrs frame)
                    prev' = frame_previnstrs frame ++ move
                in frame { frame_previnstrs = prev'
                         , frame_nextinstrs = next' }


pushData :: Data -> NBCI ()
pushData x = NBCI $ do
    frame <- gets machine_frames
    let stack = frame_stack frame
        frame' = frame { frame_stack = x : stack }
    modify $ \m -> m { machine_frames = frame' }

popData :: Int -> NBCI [Data]
popData n = NBCI $ do
    frame <- gets machine_frames
    let stack = frame_stack frame
        (it, stack') = splitAt n stack
        frame' = frame { frame_stack = stack' }
    modify $ \m -> m { machine_frames = frame' }
    return it
-- peekData ix


-- prepArg
-- flushArgsPrep
-- prepRet
-- flushRetsPrep


-- pushFrame
-- popFrame


infoIP :: NBCI (BlockName, Int)
infoIP = NBCI $ do
    frame <- gets machine_frames
    let offset = length $ frame_previnstrs frame
        name = frameinfo_name frame
    return (name, offset)