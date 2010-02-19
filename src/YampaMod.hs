{-# LANGUAGE GADTs, Rank2Types, CPP #-}
module YampaMod where

import Control.Arrow
import Control.Monad
--import FRP.Yampa.Miscellany (( # ), dup, swap)
import FRP.Yampa.Event
import FRP.Yampa.VectorSpace
import FRP.Yampa hiding (react, reactInit)
import Data.Time.Clock
import Control.Concurrent
import Data.IORef
import FRP.Yampa


data SF2 a b = SF {sfTF :: a -> Transition a b}

data SF' a b where
    SFArr   :: !(DTime -> a -> Transition a b) -> !(FunDesc a b) -> SF' a b
    -- The b is intentionally unstrict as the initial output sometimes
    -- is undefined (e.g. when defining pre). In any case, it isn't
    -- necessarily used and should thus not be forced.
    SFSScan :: !(DTime -> a -> Transition a b)
               -> !(c -> a -> Maybe (c, b)) -> !c -> b
               -> SF' a b
    SFEP   :: !(DTime -> Event a -> Transition (Event a) b)
              -> !(c -> a -> (c, b, b)) -> !c -> b
              -> SF' (Event a) b
    SFCpAXA :: !(DTime -> a -> Transition a d)
               -> !(FunDesc a b) -> !(SF' b c) -> !(FunDesc c d)
               -> SF' a d
    --  SFPair :: ...
    SF' :: !(DTime -> a -> Transition a b) -> SF' a b

-- A transition is a pair of the next state (in the form of a signal
-- function) and the output at the present time step.

type Transition a b = (SF' a b, b)

sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
sfTF' (SFArr tf _)       = tf
sfTF' (SFSScan tf _ _ _) = tf
sfTF' (SFEP tf _ _ _)    = tf
sfTF' (SFCpAXA tf _ _ _) = tf
sfTF' (SF' tf)           = tf

data FunDesc a b where
    FDI :: FunDesc a a					-- Identity function
    FDC :: b -> FunDesc a b				-- Constant function
    FDE :: (Event a -> b) -> b -> FunDesc (Event a) b	-- Event-processing fun
    FDG :: (a -> b) -> FunDesc a b			-- General function


-- NOTE: Added rsLastTime, separated ReactHandle a b out
data ReactState a b = ReactState {
    rsActuate :: Bool -> b -> IO (),
    rsSF :: SF' a b,
    rsA :: a,
    rsB :: b,
    rsLastTime :: UTCTime -- TODO: explain this better
}

type ReactChan a = Chan (a -> a, Bool)

-- initialize top-level reaction handle
reactInit :: IO a -- init
             -> (Bool -> b -> IO ()) -- actuate
             -> SF a b
             -> IO (ReactHandle a b, ReactChan a)
reactInit init actuate (SF {sfTF = tf0}) =
  do a0 <- init
     let (sf,b0) = tf0 a0
     -- TODO: really need to fix this interface, since right now we
     -- just ignore termination at time 0:
     t <- getCurrentTime
     r <- newIORef (ReactState {rsActuate = actuate, rsSF = sf,
				rsA = a0, rsB = b0, rsLastTime = t})
     chan <- newChan
     actuate True b0
     return (r,chan)

-- process a single input sample:
reactInternal :: ReactHandle a b
                 -> (a -> a)
                 -> Bool
                 -> IO ()
reactInternal rh fa bl =
  do rs@(ReactState {rsActuate = actuate,
					 rsSF = sf,
					 rsA = a,
					 rsB = b,
					 rsLastTime = t }) <- readIORef rh
     t' <- getCurrentTime
     let a' = fa a
         dt = diffUTCTime t' t
         (sf',b') = (sfTF' sf) (fromRational $ toRational $ (if dt < 0 then dt+86400 else dt)) a'
     writeIORef rh (rs {rsSF = sf',rsA = a',rsB = b',rsLastTime = t'})
     actuate bl b'

reactWriteChan :: ReactChan a -> (a -> a) -> Bool -> IO ()
reactWriteChan rch f bl = writeChan rch (f,bl)

react2 :: ReactHandle a b -> ReactChan a -> IO ()
react2 rh rch = do
    empty <- isEmptyChan rch
    when (not empty) $ do
        (f,bl) <- readChan rch
        reactInternal rh f bl
        react2 rh rch

