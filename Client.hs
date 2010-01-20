module Main where

import Network (PortID(PortNumber), connectTo, withSocketsDo)
import System (getArgs)
import Control.Monad (when)

import FRP.Yampa ((>>>), arr) -- NOTE that this is our modified YAMPA
import RunGame (glInit, runGame, game)
import Common (CSMsg'(CSMsgJoin))
import Object (serverObject, scoreboard, terrain0, renderObsObjState)
import Net (sendCSMsg)

-- Client triggers the game with:
-- ./Client <player-name> <host-name>
main :: IO ()
main = withSocketsDo $ do -- withSocketsDo is only needed for Windows platform, harmless on others
    args <- getArgs
    when (length args < 1) $ error "Wrong syntax.  Syntax: ./Client <play-name> <host-name>"
    let (playerName:serverHost:_) = args

    print ("Connecting player " ++ playerName ++ " to " ++ serverHost)

    -- Ask server to connect
    handle <- connectTo serverHost (PortNumber 4444)

    -- Tell server that this player is joining
    sendCSMsg handle $ (-1, CSMsgJoin playerName)

    -- Prepare some OpenGL intialization and windows management
    glInit

    let initialObjs = [serverObject playerName, scoreboard, terrain0]

    -- TODO: Explain runGame
    runGame (Just handle) (game initialObjs >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())

