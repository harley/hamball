module GameInput where

import FRP.Yampa
import Common
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Vec3d
import Control.Monad
import Control.Concurrent

type MyReactHandle = ReactHandle GameInput (IO (), IO ())

data GameInput = GameInput {key :: Maybe Key,
                            keyState :: Maybe KeyButtonState,
                            leftClick :: Bool,
                            mWheel :: Int,
                            posMouse :: Position,
                            message :: SCMsg}
    deriving Show

--the following OpenGL types are listed here for convenience
--data Key = Char Char | SpecialKey SpecialKey | MouseButton MouseButton
--data KeyState = Down | Up
--data Modifiers = Modifiers {shift :: KeyState, ctrl :: KeyState, alt :: KeyState}
--data Position = Position !GLint !GLint

keyboardCallback :: ReactChan GameInput -> IORef Bool -> Key -> KeyButtonState -> IO ()
keyboardCallback _ quit (SpecialKey ESC) Press = writeIORef quit True
keyboardCallback rch _ k ks = react rch (\gi -> gi {key = Just k, keyState = Just ks}) True

mouseClickCallback :: ReactChan GameInput -> MouseButton -> KeyButtonState -> IO ()
mouseClickCallback rch ButtonLeft ks = react rch (\gi -> gi {leftClick = (ks == Press)}) True
mouseClickCallback rch _ ks = react rch (\gi -> gi) True

mWheelCallback :: ReactChan GameInput -> Int -> IO ()
mWheelCallback rch i | i > 2000000000= react rch (\gi -> gi {mWheel = i-4294967296}) True --hackish because of the stupid round around coord by GLFW
                     | otherwise =     react rch (\gi -> gi {mWheel = i}) True

mouseMotionCallback :: ReactChan GameInput -> IORef Double -> IORef Int -> Position -> IO ()
mouseMotionCallback rch timerRef yPrev p@(Position x y) = do
    t' <- readIORef timerRef
    t <- get GLFW.time
    when (t-t' >= mouseTimer) $ do
        yp' <- readIORef yPrev
        let yp = fromIntegral yp'
            p = Position (if x < (width `div` 4) then x+(width `div` 2) else if x >= (3*width `div` 4) then x - (width `div` 2) else x)
                         (if y < (height `div` 3) then (height `div` 3) else if y >= (2*height `div` 3) then (2*height `div` 3 - 1) else y)
            needsUpdate = x < width `div` 4 || x >= 3*width `div` 4 || (y < height `div` 3 && y > yp) || (y >= 2*height `div` 3 && y < yp)
                                    || y < height `div` 6 || y >= 5 * height `div` 6
            performUpdate = do
                mousePos $= p
                return ()
        writeIORef yPrev $ fromIntegral y
        when needsUpdate performUpdate
        react rch (\gi -> gi {posMouse = p}) True
    writeIORef timerRef t
