module RunGame where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Yampa
import Common
import Player
import GameInput
import Object
import Vec3d
import GameCore
import IdentityList
import FRP.Yampa.Forceable
import Data.IORef
import System.IO
import Monad
import Control.Concurrent
import Network
import Laser
import Data.Maybe
import Net
import Terrain
import TerrainData
import Textures -- to load TGA file into TextureObject
--import TextureFonts
import Render

--PLEASE run ./client playerName or ./server scripts instead

game :: [ObjectSF] -> SF GameInput ([ObsObjState], [CSMsg])
game initialObjs = proc gi -> do
    oos <- loopPre emptyIL (arr dup <<< gameCore (listToIL initialObjs)) -< gi
    returnA -< (map ooObsObjState $ elemsIL oos, concatMap ooNetworkMsgs $ elemsIL oos)

runGame :: Maybe Handle -> SF GameInput (IO (), IO ()) -> IO ()
runGame handle sf = do
        t <- get GLFW.time
        sTime <- newIORef t -- start time
        ldTime <- newIORef t -- last draw time
        yPrev <- newIORef (fromIntegral $ height `div` 2) -- TODO: explain this
        nFrames <- newIORef 0

        let gd = GameData {startTime = sTime,
                           lastDrawTime = ldTime,
                           numFrames = nFrames}
        (rh,rch) <- reactInit initGameInput (actuate gd) sf
        networkInit rch handle
        tm <- newIORef t
        quit <- newIORef False

        keyCallback $= keyboardCallback rch quit
        mouseButtonCallback $= mouseClickCallback rch
        mousePosCallback $= mouseMotionCallback rch tm yPrev
        mouseWheelCallback $= mWheelCallback rch
        windowCloseCallback $= writeIORef quit True

        disableSpecial AutoPollEvent

        -- invoke drawing loop
        loop rh rch quit

        -- if quit, close server handle
        case handle of
            Just h -> do
                printFlush "Announcing exit."
                sendCSMsg h (-1, CSMsgExit)
                hClose h
            _ -> return ()

        -- finish up GLFW
        closeWindow
        terminate
    where
        loop rh rch quit = do
            sleep 0.001
            pollEvents
            empty <- isEmptyChan rch
            when empty $ reactWriteChan rch id True -- TODO: what is this for? to make sure yampa keeps updating the SF. will clean up by moving to to react
            react rh rch
            q <- readIORef quit
            unless q $ loop rh rch quit

        actuate gd shouldDraw (renderActions, networkActions) = do
            --tid <- myThreadId
            --printFlush ("draw" ++ (show tid))
            st <- readIORef $ startTime gd
            ldt <- readIORef $ lastDrawTime gd
            t <- get GLFW.time

            when (shouldDraw && t-ldt >= redrawTimer) $ do
                clear [ColorBuffer, DepthBuffer]
                renderActions
                writeIORef (lastDrawTime gd) t
                --printFlush ("Time = " ++ show (t-ldt))-- ++ (show ((fromIntegral $ nf+1)/(fromIntegral $ t-st)*1000)))
                renderOrtho widthf heightf $ do
                    blend $= Enabled
                    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)-- transparent colors will let the background show through and opaque colors will be drawn over it.
                    textureFunction $= Replace
                    renderText 5 60 ("Draw Time: " ++ show (t-ldt)) 2
                    blend $= Disabled
                swapBuffers
            networkActions
            nf <- readIORef $ numFrames gd
            writeIORef (numFrames gd) (nf+1)

glInit :: IO ()
glInit = do
    initialize

    openWindow (Size width height) [DisplayAlphaBits 8] Window --FullScreen
    -- TODO: mouse wraps horizontally well; not vertically but it's not needed anyway
    disableSpecial MouseCursor
    windowTitle $= "Hamsters Game version 0.0.2.0"
    stencilTest $= Enabled

    matrixMode $= Projection
    initFrustum -- TODO: explain this
    matrixMode $= Modelview 0

    clearColor $= Color4 0 0 0 1
    shadeModel $= Smooth

    lighting $= Enabled
    light (Light 0) $= Enabled
    depthFunc $= Just Less

    diffuse (Light 0) $= Color4 1 1 1 1
    position (Light 0) $= Vertex4 1 1 1 0

initGameInput :: IO GameInput
initGameInput = return $ GameInput {key=Nothing, keyState=Nothing, leftClick=False, posMouse=Position 0 0, mWheel = 0, message=dummySCMsg, rightClick = False}
{-
myCatch loop = do
    catch loop (\e -> do
        print "ERRPR:"
        print e
        i <- myThreadId
        killThread i
        )
-}
networkInit :: ReactChan GameInput -> Maybe Handle -> IO ()
networkInit rch Nothing = return ()
networkInit rch (Just handle) = do
    fail <- hIsClosed handle
    when fail (error "networkInit handle fail")
    fid <- forkIO $ do
        mid <- myThreadId
        printFlush (show mid)
        let loop = do
--             print ("Waiting for server message on handle " ++ (show handle))
             succ <- hWaitForInput handle (-1)
             when succ $ fetchSCMsg rch handle
             loop
        --loop
        catch loop (\e -> print "Client network thread is dying.")
    printFlush $ "forked " ++ show fid
    return ()

