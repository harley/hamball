module Main where

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
import TextureFonts
import Render

--main = playerTest

game :: [ObjectSF] -> SF GameInput ([ObsObjState], [CSMsg])
game initialObjs = proc gi -> do
    oos <- loopPre emptyIL (arr dup <<< gameCore (listToIL initialObjs)) -< gi
    returnA -< (map ooObsObjState $ elemsIL oos, concatMap ooNetworkMsgs $ elemsIL oos)

playerTest :: IO ()
playerTest = do
    glInit
    runGame Nothing (game [observer dummyPlayer, player1 1 $ Vec3d(10,0,0), terrain0] >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, return ()))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())

playerTestQuick :: IO ()
playerTestQuick = do
    glInit
    runGame Nothing (game [observer dummyPlayer, terrain1] >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, return ()))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())

textureTest :: IO ()
textureTest = do
    glInit
--    tx <- createTexture "test6.rgb" (False, False)
    tx <- getAndCreateTexture "bricks"
    runGame Nothing (game [observer dummyPlayer, texture0 $ fromJust tx] >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, return ()))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())

main = networkTest

networkTest :: IO ()
networkTest = withSocketsDo $ do
    handle <- connectTo "cicada.cs.yale.edu" (PortNumber 4444)
    glInit
    --hWaitForInput handle (-1)
    --ln <- hGetLine handle
    --let processMsg (pid,SCMsgInitialize p) = [observer pid (playerPos p)]
    runGame (Just handle) (game [serverObject, terrain0] >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())

runGame :: Maybe Handle -> SF GameInput (IO (), IO ()) -> IO ()
runGame handle sf = do
        t <- get GLFW.time
        sTime <- newIORef t
        ldTime <- newIORef t
        yPrev <- newIORef (fromIntegral $ height `div` 2)
        nFrames <- newIORef 0
        let gd = GameData {startTime = sTime,
                           lastDrawTime = ldTime,
                           numFrames = nFrames}
        (rh,rch) <- reactInit initGameInput (actuate gd) sf
        networkInit rch handle
        tm <- newIORef t
        quit <- newIORef False

        disableSpecial MouseCursor
        keyCallback $= keyboardCallback rch quit
        mouseButtonCallback $= mouseClickCallback rch
        mousePosCallback $= mouseMotionCallback rch tm yPrev
        windowCloseCallback $= writeIORef quit True

        disableSpecial AutoPollEvent
        loop rh rch quit

        closeWindow
        terminate
    where
        loop rh rch quit = do
            sleep 0.001
            pollEvents
            empty <- isEmptyChan rch
            when empty $ react rch id True
            reactCommit rh rch
            q <- readIORef quit
            unless q $ loop rh rch quit

        actuate gd shouldDraw (renderActions,networkActions) = do
            --tid <- myThreadId
            --printFlush ("draw" ++ (show tid))
            st <- readIORef $ startTime gd
            ldt <- readIORef $ lastDrawTime gd
            t <- get GLFW.time
            when (shouldDraw && t-ldt >= redrawTimer) $ do
                clear [ColorBuffer,DepthBuffer]
                renderActions
                writeIORef (lastDrawTime gd) t
                --printFlush ("Time = " ++ show (t-ldt))-- ++ (show ((fromIntegral $ nf+1)/(fromIntegral $ t-st)*1000)))
                renderOrtho widthf heightf $ do
                    printFonts' 0 60 (tex, base) 1 ("Draw Time: " ++ show (t-ldt))
                swapBuffers
            networkActions
            nf <- readIORef $ numFrames gd
            writeIORef (numFrames gd) (nf+1)

glInit :: IO ()
glInit = do
    initialize

    openWindow (Size width height) [DisplayAlphaBits 8] Window
    windowTitle $= "Hamsters Game version 0.0.2.0"
    stencilTest $= Enabled

    matrixMode $= Projection
    initFrustum
    matrixMode $= Modelview 0

    clearColor $= Color4 0 0 0 1
    shadeModel $= Smooth

    lighting $= Enabled
    light (Light 0) $= Enabled
    depthFunc $= Just Less

    diffuse (Light 0) $= Color4 1 1 1 1
    position (Light 0) $= Vertex4 1 1 1 0

initGameInput :: IO GameInput
initGameInput = return $ GameInput {key=Nothing, keyState=Nothing, leftClick=False, posMouse=Position 0 0, message=dummySCMsg}

networkInit :: ReactChan GameInput -> Maybe Handle -> IO ()
networkInit rch Nothing = return ()
networkInit rch (Just handle) = do
    fail <- hIsClosed handle
    when fail (printFlush "networkInit handle fail")
    unless fail $ do
        forkIO $ do
            let loop = do
                 --print ("Waiting for server message on handle " ++ (show handle))
                 succ <- hWaitForInput handle (-1)
                 when succ $ fetchSCMsg rch handle
                 loop
            loop
        return ()
