{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Generate client's executable                              *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module Main where

import Network (PortID(PortNumber), connectTo, withSocketsDo)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System (getArgs)
import Control.Monad (when)

import FRP.Yampa ((>>>), arr)
import RunGame (glInit, runGame, game)
import Object (serverObject, scoreboard, terrain0, renderObsObjState)
import Net (sendCSMsg)
import System.Console.GetOpt
import Common

defaultConfigs :: GameConfig
defaultConfigs = GameConfig {
    gcFullscreen = False,
    gcPlayerName = "uninitialized",
    gcTracker = "http://hamsterver.heroku.com/last"
}

options :: [OptDescr (GameConfig -> IO GameConfig)]
options = [
    Option ['f'] ["fullscreen"] (NoArg (\gc -> return gc{gcFullscreen=True} ))        "show version number",
    Option ['n'] ["name"]   (ReqArg (\arg gc -> return gc{gcPlayerName=arg}) "FILE")  "input file to read",
    Option ['t'] ["tracker"] (ReqArg (\arg gc -> return gc{gcTracker=arg}) "FILE")    "address of server tracker"
  ]

-- Use host-name only leaving it out fails.
-- Server hostname is kept track by the remote serverTracker
main :: IO ()
main = withSocketsDo $ do -- withSocketsDo is only needed for Windows platform, harmless on others
    args <- getArgs
    let ( actions, _, _) = getOpt Permute options args
    config <- foldl (>>=) (return defaultConfigs) actions

    let playerName = gcPlayerName config
    when (playerName == "uninitialized" ) $ error "Wrong syntax.  Syntax: ./Client -n <player-name>"

    -- Ask remote server tracker which server is on
    r <- simpleHTTP (getRequest $ gcTracker config)
    serverHost <- getResponseBody r
    if serverHost == "NOSERVER" then error "No server is open." else return serverHost

    print ("Connecting player " ++ playerName ++ " to " ++ serverHost)

    -- Ask server to connect
    handle <- connectTo serverHost (PortNumber 4444)

    -- Tell server that this player is joining
    sendCSMsg handle $ (-1, CSMsgJoin playerName)

    -- Prepare some OpenGL intialization and windows management
    glInit config

    let initialObjs = [serverObject playerName, scoreboard, terrain0]

    -- TODO: Explain runGame
    runGame config (Just handle) (game initialObjs >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())

