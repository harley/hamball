{-# LANGUAGE Arrows #-}
{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Server code, handling messages & some collision detection *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module Main where

import FRP.Yampa
import Vec3d
import Common
import Collision
import Data.Maybe
import Network
import Network.BSD (getHostName)
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)
import Control.Concurrent
import System.IO
import IdentityList
import Monad
import Data.List
import System.IO.Error
--import System (getArgs)
import Net
import Data.Time.Clock

-- PlayerID is now name not ID, but server also labels player with ID to use internally
data ServerState = ServerState {handles :: ![(Int, Handle)],
                                nextID :: !Int,
                                lastExitID :: !Int,
                                allPlayers :: ![Player],
                                allLasers :: !(IL Laser)}--TODO: why IL on Lasers and why not on Players
    deriving (Show, Eq)

emptyServerState :: ServerState
emptyServerState = ServerState{handles=[], nextID=0, lastExitID=(-1), allPlayers=[], allLasers=emptyIL}

serverTracker :: String
serverTracker = "http://hamsterver.heroku.com/"

data ServerInput = ServerInput {msg :: !CSMsg,
                                handle :: !(Maybe Handle)}
    deriving (Show, Eq)

-- TODO: rename dummy to empty for consistency?
dummyServerInput :: ServerInput
dummyServerInput = ServerInput {msg = dummyCSMsg, handle = Nothing}

main :: IO ()
main = do
    -- Tell the online server tracker that I am open and able to accept request
    hostName <- getHostName
    putStrLn $ "Informing server tracker about host " ++ hostName
    r <- simpleHTTP $ getRequest (serverTracker ++ "open?name=" ++ hostName)
    txt <- getResponseBody r
    putStrLn txt

    runServer (PortNumber 4444) server

--    r <- simpleHTTP $ getRequest (serverTracker ++ "close?name=" ++ hostName)
--    txt <- getResponseBody r
--    putStrLn txt

runServer :: PortID -> SF ServerInput (IO()) -> IO ()
runServer port sf = withSocketsDo $ do
          sock <- listenOn port

          rh <- reactInit (return dummyServerInput) (\_ _ sendmsgs -> sendmsgs >> return False) sf
          rch <- newChan

          -- one thread listens for new players joining
          forkIO $ acceptClient rch sock

          -- write to chan once in a while to keep the server hard at work, so that server is updated
          forkIO $ do
                let loop = do
                      addToReact rch id
                      threadDelay 1000    -- Microseconds
                      loop
                loop

          startTime <- getCurrentTime
          -- main thread process.
          -- this readChan/unGetChan makes it more efficent (server idle instead of looping)
          let loop lTime lastA = do
                newA <- getReactInput rch lastA
                curTime <- getCurrentTime
                react rh (fromRational . toRational $ diffUTCTime curTime lTime, Just newA)
                loop curTime newA
          loop startTime dummyServerInput
      where acceptClient rch sock = do
                (hand,_,_) <- accept sock
                open <- hIsOpen hand
                printFlush ("Accepting, verify opened: " ++ show open)
                forkIO $ do
                    let loop = do
                           succ <- hWaitForInput hand (-1)
                           when succ $ fetchCSMsg rch hand
                           loop
                    -- When player quits, handle becomes invalid (closed by main thread), thus exception thrown
                    catch loop (\e -> print "Player quit.")
                acceptClient rch sock

initializePlayer :: ID -> String -> Player
initializePlayer pid name = Player {playerID = pid,
                                    playerPos = Vec3d (25*(fromIntegral pid),0,0),
                                    playerVel = Vec3d (0,0,0),
                                    playerAcc = Vec3d (0,0,0),
                                    playerView = (0,0),
                                    playerRadius = defRadius,
                                    playerLife = maxLife,
                                    playerEnergy = maxEnergy,
                                    playerColor = let pid' = fromIntegral (pid+2)
                                                  in Vec3d (0.5, 0.2*pid' - (fromIntegral $ floor $ 0.2*pid'), 0.1*pid' - (fromIntegral $ floor $ 0.1*pid')),
                                    playerName = name}

server :: SF ServerInput (IO())
server = proc si -> do
    msgs <- loopPre (emptyServerState, emptyServerState) objSF -< si
    returnA -< msgs

fetchCSMsg :: ReactChan ServerInput -> Handle -> IO ()
fetchCSMsg rch h = do
    ln <- hGetLine h
    addToReact rch (\si -> si {msg = destringify ln, handle = Just h})

sendSCMsg :: Handle -> SCMsg -> IO ()
sendSCMsg h msg = do
    --_ <- hIsOpen h  -- The game breaks if we uncomment this line! WTFWTFWTFWTFTWFFFFFFFFFFFFFFFFFFFF
    hPutStrLn h $ stringify msg -- debugShow (stringify msg)
    hFlush h

objSF :: SF (ServerInput, (ServerState, ServerState)) (IO(), (ServerState, ServerState))
objSF = proc (si, (sprev, s0)) -> do
    --inputChange <- loopPre dummyServerInput detectChangeSF -< si
    inputChange <- edgeBy (\old new -> if old/=new then Just new else Nothing) dummyServerInput -< si

    let s1 = updateServerState (s0,inputChange)

    lps <- moveObjs allLasers laserPos laserVel -< s1           -- calc current lasers' pos
    let s2 = s1 {allLasers = zipWithIL (\l p -> l {laserPos = p}) (const Nothing) (const Nothing) (allLasers s1) lps}
        hits = checkHits (sprev, s2)
        scMsgs = toMessages (s2, inputChange, hits, [])

    returnA -< (sendAll s2 scMsgs, (s2, s1))

sendAll :: ServerState -> [SCMsg] -> IO()
sendAll s msgs = fst $ foldl' (\(io,hndls) msg -> let (io',hndls') = sendMsg (msg,hndls)
                                                  in (io >> io', hndls'))
                             (return (),handles s) (mergeSort msgs)
    where
        sendMsg ((ident,msg),(hi,h):hndls) = case (ident == hi) of
                                                 True -> (sendSCMsg h (ident,msg), (hi,h):hndls)
                                                 False -> sendMsg ((ident,msg),hndls)
        sendMsg (_,[]) = (return (), [])

        mergeSort [] = []
        mergeSort [x] = [x]
        mergeSort l = let (l1,l2) = foldl (\(l1,l2) a -> (l2,a:l1)) ([],[]) l
                      in merge (mergeSort l1) (mergeSort l2)
            where merge [] l = l
                  merge l [] = l
                  merge l1@(x:xs) l2@(y:ys) = if fst x < fst y then x:(merge xs l2) else y:(merge l1 ys)

moveObjs :: (ServerState -> IL a) -> (a -> Position3) -> (a -> Velocity3) -> SF ServerState (IL Position3)
moveObjs listFun posFun velFun = proc s0 -> do
    dPs <- integral -< fmap velFun $ listFun s0
    returnA -< (fmap posFun $ listFun s0) ^+^ dPs

updateServerState :: (ServerState, Event ServerInput) -> ServerState
updateServerState (s, Event ServerInput{msg=(_, CSMsgPlayer p)})               = s{allPlayers = map (\x->if playerID x == playerID p then p else x) $ allPlayers s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgUpdate p)})               = s{allPlayers = map (\x->if playerID x == playerID p then p else x) $ allPlayers s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgLaser l)})                = s{allLasers  = insertIL l $ allLasers s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgKillLaser lid)})          = s{allLasers  = filterIL ((/= lid) . laserID) $ allLasers s}
updateServerState (s, Event ServerInput{msg=(pid, CSMsgDeath h)})  = s{allPlayers = reInitDead pid (allPlayers s)}
    where reInitDead pid (p:ps) = if playerID p == pid then (initializePlayer pid (playerName p)):ps else p:reInitDead pid ps
          reInitDead _ _ = []
updateServerState (s, Event ServerInput{msg = (_, CSMsgExit exitPlayerName), handle = Just hand})  =
    let newHandles  = filter (\(pid, h) -> h /= hand)  $ handles s
        (newPlayers, [exitPlayer])  =  partition (\p -> playerName p /= exitPlayerName) $ allPlayers s
    in s{allPlayers = newPlayers, handles = newHandles, lastExitID = playerID exitPlayer}
updateServerState (s, Event ServerInput{msg=(_, CSMsgJoin name),handle=Just hand})                 =
    let pid       = nextID s
        newPlayer = initializePlayer pid name
    in s{handles  = handles s ++ [(pid,hand)], nextID = pid+1, allPlayers = allPlayers s ++ [newPlayer]}
updateServerState (s, NoEvent) = s
updateServerState (s, _)       = error $ "updateServerState couldn't find a match for " ++ (show s)

-- Data.Maybe catMaybes :: [Maybe a] -> [a]
-- TODO: bug: if yampa rate is too low, fail to detect collisions occasionally
checkHits :: (ServerState, ServerState) -> [Hit]
checkHits (sprev, s) = catMaybes $ map collisionLP [(lprev,l,p) | (lprev,l) <- map snd $ assocsIL $
                                                                     zipWithIL (\a b -> (a,b)) (const Nothing) (const Nothing)
                                                                               (allLasers sprev) (allLasers s),
                                                                  p <- allPlayers s, laserpID l /= playerID p]
{-
checkCollisions :: ServerState -> [Player]
checkCollisions s = flatten $ catMaybes $ map collisionPP [(p1,p2) | p1 <- allPlayers s,
                                                                     p2 <- allPlayers s,
                                                                     playerID p1 < playerID p2] -- to avoid duplicates
    where flatten ((p1,p2):ps) = debug ("p1 = " ++ (show $ playerID p1) ++ " && p2 = " ++ (show $ playerID p2)) $ [p1,p2] ++ (flatten ps)
          flatten [] = []

collisionUpdates :: (ServerState, [Player]) -> ServerState
collisionUpdates (s, colliders) = s{allPlayers = map (getCol colliders) $ allPlayers s}
    where getCol :: [Player] -> Player -> Player
          getCol (c:cs) p = if playerID c == playerID p then c else getCol cs p
          getCol [] p = p

updatePlayersPos :: (ServerState, [Position3]) -> ServerState
updatePlayersPos (s, posList) = s{allPlayers = playersList}
    where playersList = zipWith (\pos p -> p{playerPos = pos}) posList (allPlayers s)
-}
toMessages :: (ServerState, Event ServerInput, [Hit], [Player]) -> [SCMsg]
toMessages (s, esi, hits, collisions) =
    let allIDs = map playerID $ allPlayers s
        -- TODO: remove colIDs
        colIDs = map playerID $ collisions
        playerUpdates = event []
                        (\si -> case msg si of -- player updates (exclude sender from recips, colliding players from list)
                            (pid, CSMsgPlayer p) -> [(i, SCMsgPlayer p) | i <- allIDs, i /= pid, pid `notElem` colIDs]
                            (pid, CSMsgLaser l ) -> [(i, SCMsgSpawn (LaserObj l)) | i <- allIDs, i /= pid]
                            (pid, CSMsgDeath h) -> let pl = case find ((pid ==) . playerID) (allPlayers s) of
                                                                            Nothing -> error "Couldn't find a player that was just killed...???"
                                                                            Just p -> p
                                                       {-pl' = case find ((player1ID h ==) . playerID) (allPlayers s) of
                                                                            Nothing -> error "Couldn't find a player that just killed someone...???"
                                                                            Just p' -> p'
                                                        -}
                                                   in [(i, SCMsgSpawn (PlayerObj pl)) | i <- allIDs, i /= playerID pl] ++
                                                      [(i, SCMsgFrag h) | i <- allIDs] ++
                                                      [(pid, SCMsgInitialize pl)]
                            (_, CSMsgJoin _) -> let pl = head $ reverse $ allPlayers s
                                                  in [(playerID pl, SCMsgInitialize pl)] ++
                                                     [(playerID pl, SCMsgSpawn (PlayerObj p)) | p <- allPlayers s, playerID p /= playerID pl] ++
                                                     [(i, SCMsgSpawn (PlayerObj pl)) | i <- allIDs, i /= playerID pl]
                            -- Send SCMsgExit so client can delete the object?
                            (_, CSMsgExit name) -> [(i, SCMsgRemove (lastExitID s)) | i <- allIDs]

                            _ -> []) esi
     in (playerUpdates ++ [(i, SCMsgHit h) | i <- allIDs, h <- hits] -- hit broadcasts
                       ++ [(i, SCMsgPlayer p) | i <- allIDs, p <- collisions]) -- collision broadcasts

