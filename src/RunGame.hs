{-# LANGUAGE Arrows, BangPatterns #-}
{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Integrate OpenGL, GLFW and Networking code all together   *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module RunGame where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Yampa
import GameInput
import Object
import GameCore
import IdentityList
import Data.IORef
import System.IO
import Monad
import Control.Concurrent
import Data.Maybe
import Net
import Render
import Common
import Data.Time.Clock
import Text.Printf (printf)

-- Information about the global state of the game
data GameData = GameData {
    startTime :: IORef Double,
    lastDrawTime :: IORef Double,
    numFrames :: IORef Int
}

defaultConfigs :: GameConfig
defaultConfigs = GameConfig {
    gcFullscreen = False,
    gcPlayerName = "uninitialized",
    gcTracker = "http://hamsterver.heroku.com/last"
  }

-- Number of milliseconds between redraws
redrawTimer :: Double
redrawTimer = 0.005

game :: [ObjectSF] -> SF GameInput ([ObsObjState], [CSMsg])
game initialObjs = proc gi -> do
    oos <- loopPre emptyIL (arr dup <<< gameCore (listToIL initialObjs)) -< gi
    returnA -< (map ooObsObjState $ elemsIL oos, concatMap ooNetworkMsgs $ elemsIL oos)

runGame :: GameConfig -> Maybe Handle -> SF GameInput (IO (), IO ()) -> IO ()
runGame GameConfig{gcPlayerName=playerName, gcFullscreen=fullscreen} handle gameSF = do
        -- Use IORef here because they are updated via OpenGL callbacks
        t <- get GLFW.time
        sTime <- newIORef t -- start time
        ldTime <- newIORef t -- last draw time
        yPrev <- newIORef (fromIntegral $ height `div` 2) -- y coord, to wrap mouse around
        nFrames <- newIORef 0

        let gd = GameData {startTime = sTime,
                           lastDrawTime = ldTime,
                           numFrames = nFrames}
        rch <- newChan
        rh <- reactInit (return initGameInput) (actuate gd) gameSF

        -- spawn ListenToServer thread to listen for network messages from Server        
        networkInit rch handle

        tm <- newIORef t
        quit <- newIORef False

        keyCallback $= keyboardCallback rch quit
        mouseButtonCallback $= mouseClickCallback rch
        mousePosCallback $= mouseMotionCallback rch tm yPrev
        mouseWheelCallback $= mWheelCallback rch
        windowCloseCallback $= writeIORef quit True

        disableSpecial AutoPollEvent

        startTime <- getCurrentTime
        
        -- invoke drawing loop
        -- process the game by clearing up the queued network messages, compute game state
        loop rh rch quit startTime initGameInput

        -- if quit, close server handle
        case handle of
            Just h -> do
                printFlush "Announcing exit."
                sendCSMsg h (-1, CSMsgExit playerName)
                hClose h
            _ -> return ()

        -- finish up GLFW
        closeWindow
        terminate
    where
        loop rh rch quit lTime curA = do
            sleep 0.001
            pollEvents
            curTime <- getCurrentTime
            let reactLoop cA = do
                empty <- isEmptyChan rch
                let dt = fromRational.toRational $ diffUTCTime curTime lTime
                if empty
                    then (react rh (dt, Nothing) >> return cA)
                    else (do
                        newA <- getReactInput rch cA
                        react rh (dt, Just newA)
                        reactLoop newA)
            nA <- reactLoop curA
            q <- readIORef quit
            unless q $ loop rh rch quit curTime nA

        actuate gd rch shouldDraw (renderActions, networkActions) = do
            -- Update via IORef because actuate is called in a callback fashion
            st <- readIORef $ startTime gd
            ldt <- readIORef $ lastDrawTime gd
            t <- get GLFW.time

            when (shouldDraw && t-ldt >= redrawTimer) $ do
                clear [ColorBuffer, DepthBuffer]
                renderActions
                writeIORef (lastDrawTime gd) t
                renderText2D 5 60 (printf "Draw Time: %.4f" (t-ldt)) 2
                swapBuffers
            networkActions
            nf <- readIORef $ numFrames gd
            writeIORef (numFrames gd) (nf+1)
            return True

glInit :: GameConfig -> IO ()
glInit GameConfig{gcFullscreen=fullscreen} = do
    initialize

    openWindow (Size width height) [DisplayAlphaBits 8] (if fullscreen then FullScreen else Window)
    disableSpecial MouseCursor
    windowTitle $= "Hamsters Balls Game version v0.2"
    stencilTest $= Enabled

    matrixMode $= Projection
    initFrustum -- set up player's view
    matrixMode $= Modelview 0

    clearColor $= Color4 0 0 0 1
    shadeModel $= Smooth

    lighting $= Enabled
    light (Light 0) $= Enabled
    depthFunc $= Just Less

    diffuse (Light 0) $= Color4 1 1 1 1
    position (Light 0) $= Vertex4 1 1 1 0

initGameInput :: GameInput
initGameInput = GameInput {key=Nothing, keyState=Nothing, leftClick=False, posMouse=Position 0 0, mWheel = 0, message=dummySCMsg, rightClick = False}

-- spawn ListenToServer thread in here
networkInit :: ReactChan GameInput -> Maybe Handle -> IO ()
networkInit rch Nothing = return ()
networkInit rch (Just handle) = do
    fail <- hIsClosed handle
    when fail (error "networkInit handle fail")
    fid <- forkIO $ do
        let loop = do
--             print ("Waiting for server message on handle " ++ (show handle))
             succ <- hWaitForInput handle (-1)
             when succ $ fetchSCMsg rch handle
             loop
        --loop
        catch loop (\e -> print "Client network thread is dying.")
    return ()

