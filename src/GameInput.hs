{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Input handling via OpenGL callbacks, used in Client       *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module GameInput where

import Common
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Monad

data GameInput = GameInput {key :: Maybe Key,
                            keyState :: Maybe KeyButtonState,
                            leftClick :: Bool,
                            mWheel :: Int,
                            posMouse :: Position,
                            rightClick :: Bool,
                            message :: SCMsg}
    deriving Show

-- Number of milliseconds between mouseUpdates
mouseTimer :: Double
mouseTimer = 0.001

--the following OpenGL types are listed here for convenience
--data Key = Char Char | SpecialKey SpecialKey | MouseButton MouseButton
--data KeyState = Down | Up
--data Modifiers = Modifiers {shift :: KeyState, ctrl :: KeyState, alt :: KeyState}
--data Position = Position !GLint !GLint

keyboardCallback :: ReactChan GameInput -> IORef Bool -> Key -> KeyButtonState -> IO ()
keyboardCallback _ quit (SpecialKey ESC) Press = writeIORef quit True
keyboardCallback rch _ k ks = addToReact rch (\gi -> gi {key = Just k, keyState = Just ks})

mouseClickCallback :: ReactChan GameInput -> MouseButton -> KeyButtonState -> IO ()
mouseClickCallback rch ButtonLeft ks = addToReact rch (\gi -> gi {leftClick = (ks == Press)})
mouseClickCallback rch ButtonRight ks = mouseWheel $= 0 >> addToReact rch (\gi -> gi {rightClick = (ks == Press), mWheel = 0})
mouseClickCallback rch _ _ = addToReact rch (\gi -> gi)

-- Make sure wheel value does not go to far
-- default range is 0 -> 4294967295, so we only use first 4 and last 4 values
mWheelCallback :: ReactChan GameInput -> Int -> IO ()
mWheelCallback rch i | i == 9  = mouseWheel $= 8
                     | i == 4294967287 = mouseWheel $= 4294967288 -- rep -4
                     | otherwise = addToReact rch (\gi -> gi {mWheel = i})

mouseMotionCallback :: ReactChan GameInput -> IORef Double -> IORef Int -> Position -> IO ()
mouseMotionCallback rch timerRef yPrev (Position x y) = do
    t' <- readIORef timerRef
    t <- get GLFW.time
    when (t-t' >= mouseTimer) $ do
        yp' <- readIORef yPrev
        let yp = fromIntegral yp'
            -- NOTE: this wraps mouse position horizontally
            -- Like in othere FPS game, does not wrap mouse vertically
            p = Position (if x < (width `div` 4) then x+(width `div` 2) else if x >= (3*width `div` 4) then x - (width `div` 2) else x)
                         (if y < (height `div` 3) then (height `div` 3) else if y >= (2*height `div` 3) then (2*height `div` 3 - 1) else y)
            needsUpdate = x < width `div` 4 || x >= 3*width `div` 4 || (y < height `div` 3 && y > yp) || (y >= 2*height `div` 3 && y < yp)
                                    || y < height `div` 6 || y >= 5 * height `div` 6
            performUpdate = mousePos $= p
        writeIORef yPrev $ fromIntegral y
        when needsUpdate performUpdate
        addToReact rch (\gi -> gi {posMouse = p})
    writeIORef timerRef t

