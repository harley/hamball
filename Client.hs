module Main where

import Network
import RunGame
import FRP.Yampa
import Terrain
import Common
import Object
import Net
import System (getArgs)
import Control.Monad
import System.IO

main :: IO ()
main = withSocketsDo $ do
    serverHost <- readFile hostName
    print serverHost

    args <- getArgs
    when (null args) $ error "Please specify your player name"
    let playerNameStr = head args

    handle <- connectTo serverHost (PortNumber 4444)
    sendCSMsg handle $ (-1, CSMsgJoin playerNameStr)
    glInit
    let initialObjs = [serverObject playerNameStr, scoreboard, terrain0]
    runGame (Just handle) (game initialObjs >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())
