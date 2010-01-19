module Main where

import Sprites
import Data.IORef

msInterval :: Int
msInterval = 16

rotSpeed :: GLfloat
rotSpeed = 2

main = do
	(progName, _) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer, WithAlphaComponent]
	makeNewWindow "HOpenGL Spriting Demo"
	mainLoop

display :: IORef GLfloat -> IORef Int -> Maybe TextureObject -> IO ()
display angle frame tex1 = do
	clear [ColorBuffer, DepthBuffer]
	angle' <- readIORef angle
	frame' <- readIORef frame
	displaySprite tex1 (304, 304) (560, 560) (0, 0) (1, 1) angle'
	displaySprite tex1 (32, 32) (64, 64) (0, 0) (1, 1) (-angle')
	flush
	swapBuffers

getCursorsFrame :: Int -> ((GLfloat, GLfloat), (GLfloat, GLfloat))
getCursorsFrame frame
	| frame < 3 = (((fromIntegral frame) * (1 / 4), 0), ((fromIntegral frame) * (1 / 4) + (1 / 4), 1))
	| frame >= 3 = (((3 / 4) - (1 / 4) * ((fromIntegral frame) - 3), 0), (1 - (1 / 4) * ((fromIntegral frame) - 3), 1))

makeNewWindow :: String -> IO ()
makeNewWindow name = do
	createWindow name
	windowSize $= Size 800 600
	spriteInit 800 600
	clearColor $= Color4 0.2 0.3 0.6 0.0
	tex1 <- createTexture "test6.rgb" (False, False)
	angle <- newIORef (0 :: GLfloat)
	frame <- newIORef (0 :: Int)
	displayCallback $= display angle frame tex1
	addTimerCallback msInterval (timer angle frame tex1)

timer :: IORef GLfloat -> IORef Int -> Maybe TextureObject -> IO ()
timer angle frame textures = do
	modifyIORef angle (\num -> if (num + rotSpeed >= 360.0) then (360 - (num + rotSpeed)) else (num + rotSpeed))
	modifyIORef frame (\f -> if (f == 5) then 0 else (f + 1))
	display angle frame textures
	addTimerCallback msInterval (timer angle frame textures)
