module Client where

import Network (PortID(PortNumber), connectTo, withSocketsDo)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System (getArgs)
import Control.Monad (when)

import FRP.Yampa ((>>>), arr) -- NOTE that this is our modified YAMPA
import RunGame (glInit, runGame, game)
import Common (CSMsg'(CSMsgJoin))
import Object (serverObject, scoreboard, terrain0, renderObsObjState)
import Net (sendCSMsg)

-- Client triggers the game with:
-- ./Client <player-name> <optional:host-name>
-- Use host-name only leaving it out fails.
-- Server hostname is kept track by the remote serverTracker
main :: IO ()
main = withSocketsDo $ do -- withSocketsDo is only needed for Windows platform, harmless on others
    args <- getArgs
    when (null args ) $ error "Wrong syntax.  Syntax: ./Client <play-name>"

    let playerName = head args
    serverHost <- if (null (tail args))
                  then do
                    -- Ask remote server tracker which server is on
                    r <- simpleHTTP (getRequest "http://hamsterver.heroku.com/last")
                    sh <- getResponseBody r
                    if sh == "NOSERVER" then error "No server is open." else return sh
                  else
                    -- Game server is specified, then use it
                    return (args !! 1)
    let serverHostFull = serverHost ++ ".zoo.cs.yale.edu"

    print ("Connecting player " ++ playerName ++ " to " ++ serverHostFull)

    -- Ask server to connect
    handle <- connectTo serverHostFull (PortNumber 4444)

    -- Tell server that this player is joining
    sendCSMsg handle $ (-1, CSMsgJoin playerName)

    -- Prepare some OpenGL intialization and windows management
    glInit

    let initialObjs = [serverObject playerName, scoreboard, terrain0]

    -- TODO: Explain runGame
    runGame playerName (Just handle) (game initialObjs >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())

