module Object where

import Common
import FRP.Yampa
import GameInput
import Player
import Laser
import Terrain
import TerrainData
import Vec3d
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW hiding (time)
import Data.Maybe
import BoundingVolume
import Render
import System.Random
import System.IO.Unsafe
import Particles

type ObjectSF = SF ObjInput ObjOutput

data ObjInput = ObjInput {
    oiGameInput :: !GameInput,
    oiColliding :: !(Maybe ObjOutput)
}

data ObjOutput = ObjOutput {
    ooObsObjState :: !ObsObjState,         -- Observable Object State
    ooNetworkMsgs :: ![CSMsg],             -- Messages to send to the server
    ooKillReq :: !(Event ()),              -- When this happens, kill the object itself
    ooSpawnReq :: ![ObjectSF],             -- Spawn these guys pls
    ooBounds :: !BoundingVolume            -- For collision detection on the client side, currently player vs player
}

instance Eq ObjOutput where
    (==) _ _ = error "(==) called on ObjOutputs"
    (/=) _ _ = error "(/=) called on ObjOutputs"

data ObsObjState = OOSPlayer !Player
                 | OOSLaser !Laser
                 | OOSSelf !Player
                 | OOSTerrain ![TerrainElement]
                 | OOSParticle !Particle
                 | OOSKillText !String
                 | OOSPowerUp !PowerUp
                 | OOSScoreBoard !ScoreBoard
                 | OOSMan -- not necessarily to be used
                 | OOSNone
    deriving Show

-- Waterfall
terrainW :: ObjectSF
terrainW = proc ObjInput {oiGameInput = gi} -> do
    kill <- edge <<^ (\gi -> key gi == Just (CharKey 'K')) -< gi
    returnA -< ObjOutput {ooObsObjState=OOSTerrain [waterfall],
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = [],
                          ooBounds = BoundingEmpty}

-- Main game terrain
terrain0 :: ObjectSF
terrain0 = proc oi -> do
    returnA -< ObjOutput {ooObsObjState=OOSTerrain [demoTerrain],
                          ooNetworkMsgs = [],
                          ooKillReq = NoEvent,
                          ooSpawnReq = [],
                          ooBounds = getTerrainBounds demoTerrain}


renderObsObjState :: ObsObjState -> IO ()
renderObsObjState (OOSPlayer p) = renderPlayer p
renderObsObjState (OOSLaser l) = renderLaser l
renderObsObjState (OOSSelf p) = renderSelf p
renderObsObjState (OOSTerrain ts) = (foldr (>>) (return ()) $ map renderTerrainElement ts)
renderObsObjState (OOSParticle p) = renderParticle p
renderObsObjState (OOSKillText str) = renderKillText str
renderObsObjState (OOSScoreBoard sb) = renderScoreBoard sb
renderObsObjState (OOSPowerUp pow) = renderPlayer $ Player {playerID = 0,
                                                            playerPos = powerupPos pow,
                                                            playerVel = zeroVector,
                                                            playerAcc = zeroVector,
                                                            playerView = (0,0),
                                                            playerRadius = defRadius,
                                                            playerLife = maxLife,
                                                            playerEnergy = maxEnergy,
                                                            playerColor = Vec3d(0.5, 0.2, 0.7),
                                                            playerName = "Dummy"}
renderObsObjState OOSNone = return ()

-- observer is the player that is being controlled at each client
-- TODO: Improve and clean this up
observer :: Player -> ObjectSF
observer pl = let setFromKey k (gi, prev) = dup $ case (key gi == Just k, keyState gi) of
                                                                (True,Just Press) -> 1
                                                                (True,Just Release) -> 0
                                                                (_   ,_        ) -> prev
                  checkMouseWheel (gi, prev)  =  let raw = mWheel gi
                                                     -- to avoid overflow or casting to big integer 
                                                     v = if raw < 9 then raw else (raw - 4294967295)-1 
                                                     actualV = float v / 4 --if rightClick gi then 0 else float v / 4
                                                 in dup actualV
                  speed = 20
                  getx GameInput{posMouse=Position x y} = x
                  gety GameInput{posMouse=Position x y} = y

                  powerupSF' val = switch (arr ((+val) . fst) &&& after 15 ()) (\_ -> powerupSF)
                  powerupSF = switch (arr fst &&& arr snd) powerupSF'

               in proc (ObjInput {oiGameInput = gi, oiColliding = collider}) -> do

    let theta = sensitivity * (fromIntegral $ getx gi - (width `div` 2))
        phi = (-sensitivity) * (fromIntegral $ gety gi - (height `div` 2))
        f' = Vec3d (cos theta * cos phi, -sin theta * cos phi, sin phi)
        r' = Vec3d (-cos (theta-pi/2),sin (theta-pi/2), 0)
        f = speed *^ f'
        r = speed *^ r'
        up = speed *^ (r' `cross` f') -- actually r cross f can be calculated by hand. im too lazy for now

    fwd  <- loopPre 0 $ arr $ setFromKey (CharKey 'W') -< gi
    bwd <- loopPre 0 $ arr $ setFromKey (CharKey 'S') -< gi
    right <- loopPre 0 $ arr $ setFromKey (CharKey 'D') -< gi
    left <- loopPre 0 $ arr $ setFromKey (CharKey 'A') -< gi
    keyWheel <- loopPre 0 $ arr $ checkMouseWheel -< gi
    stopEvent <- edge -< rightClick gi
    collideEvent <- edge <<^ (/= Nothing) -< collider
    let df = (fwd - bwd) *^ f
        dr = (right - left) *^ r
        du = keyWheel *^ up
        a = df ^+^ dr ^+^ du
        v = (if isEvent collideEvent then -5 else 1) *^ a
    --v <- integral -< a
    p <- (playerPos pl ^+^) ^<< integral -< v

    let pow = case (collideEvent, collider) of
                  (Event (), Just (ObjOutput{ooObsObjState=OOSPowerUp pw})) -> Just pw
                  _ -> Nothing

        laserPowEvent = maybe NoEvent (\pow -> case powerupType pow of
                                                   StrengthenLaser str -> Event str
                                                   _ -> NoEvent) pow
        radiusPowEvent = maybe NoEvent (\pow -> case powerupType pow of
                                                    DecreaseRadius r -> Event (-r)
                                                    _ -> NoEvent) pow

    laserstr <- powerupSF -< (defLaserStr, laserPowEvent)
    radius   <- powerupSF -< (playerRadius pl, radiusPowEvent)

    t <- time -< ()
    let lsr = Laser {laserID = round (10*t),
                     laserpID = playerID pl,
                     laserPos = p,
                     laserVel = 20 *^ f,
                     laserStr = laserstr,
                     laserColor = playerColor pl}

    fireLaser <- edge -< leftClick gi

    changeVel <- tagWith () ^<< loopPre (Vec3d (0,0,0)) detectChangeSF -< v
    changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    let hitEvent = case changeMsg of
                       Event (_,SCMsgHit h) -> if playerID pl == player2ID h then changeMsg else NoEvent
                       _ -> NoEvent

    life <- loopPre (playerLife pl) (arr (\(hev,life) -> dup $ maybeEvent life (\(_,SCMsgHit h) -> life - hitStr h) hev)) -< hitEvent
    let killerID = case hitEvent of
                       Event (_,SCMsgHit h) -> player1ID h
                       _ -> -1
        kill = if life <= 0 then Event killerID else NoEvent
        pl' = pl {playerPos = p,
                  playerVel = v,
                  playerAcc = a,
                  playerView = (theta,phi),
                  playerLife = life,
                  playerRadius = radius}
    returnA -< ObjOutput {ooObsObjState = OOSSelf pl',
                          ooNetworkMsgs = (\xs -> [x | Event x <- xs]) $
                                            [fireLaser `tag` (playerID pl,CSMsgLaser lsr),
                                             foldl (mergeBy const) NoEvent [changeVel, hitEvent `tag` ()] `tag` (playerID pl, CSMsgPlayer pl'),
                                             fmap (\pid -> (playerID pl, CSMsgDeath pid)) kill],
                          ooKillReq = kill `tag` (),
                          ooSpawnReq = maybeEvent [] (\_ -> [laser lsr]) fireLaser,
                          ooBounds = let v = Vec3d (playerRadius pl', playerRadius pl', playerRadius pl')
                                     in BoundingBox (p ^-^ v) (p ^+^ v)}
{-
player0 :: ID -> Velocity3 -> ObjectSF
player0 pid p0 = let setFromKey k (gi, prev) = (\a -> (a,a)) $ (case (key gi == Just k, keyState gi) of
                                                                 (True,Just Press) -> 1
                                                                 (True,Just Release) -> 0
                                                                 (_   ,_        ) -> prev)
                  in proc (ObjInput {oiGameInput = gi}) -> do
    (f,r,u) <- arr id -< (Vec3d (0,0,-0.001), Vec3d (0.001,0,00), Vec3d (0,0.001,0))
    keyw <- loopPre 0 $ arr $ setFromKey (CharKey 'I') -< gi
    keys <- loopPre 0 $ arr $ setFromKey (CharKey 'K') -< gi
    keyd <- loopPre 0 $ arr $ setFromKey (CharKey 'L') -< gi
    keya <- loopPre 0 $ arr $ setFromKey (CharKey 'J') -< gi
    arup <- loopPre 0 $ arr $ setFromKey (SpecialKey UP) -< gi
    ardn <- loopPre 0 $ arr $ setFromKey (SpecialKey DOWN) -< gi
    let df = (keyw-keys) *^ f
        dr = (keyd-keya) *^ r
        du = (arup-ardn) *^ u
        a = df ^+^ dr ^+^ du
    v <- integral -< a
    p <- (p0 ^+^) ^<< integral -< v
    msg <- arr id -< message gi
    returnA -< let pUpdate = Player {playerID = pid,
                                     playerPos = p,
                                     playerVel = v,
                                     playerAcc = a,
                                     playerView = (0,0),
                                     playerRadius = defRadius,
                                     playerLife = maxLife,
                                     playerEnergy = maxEnergy,
                                     playerColor = Vec3d (0.5,0.5,0.5),
                                     playerName = "player0"}
                in ObjOutput {ooObsObjState = case msg of (iden, SCMsgPlayer p) -> if pid == iden then OOSPlayer p else OOSPlayer pUpdate
                                                          _ -> OOSPlayer pUpdate,
                              ooNetworkMsgs = [],
                              ooKillReq = NoEvent,
                              ooSpawnReq = [],
                              ooBounds = BoundingEmpty}
-}
serverObject :: String -> ObjectSF
serverObject playerNameStr = proc (ObjInput {oiGameInput = gi}) -> do
    spawnWaterfall <- edge <<^ (\gi -> key gi == Just (CharKey 'L')) -< gi
    changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    let processSCMsg NoEvent = ObjOutput {ooObsObjState = OOSNone, ooNetworkMsgs = [], ooKillReq = NoEvent, ooSpawnReq = [], ooBounds = BoundingEmpty}
        processSCMsg (Event (_,msg)) = ObjOutput {ooObsObjState = OOSNone, ooNetworkMsgs = [],
                                                  ooKillReq = NoEvent,
                                                  ooSpawnReq = case msg of
                                                                   SCMsgSpawn (PlayerObj p) -> [player p dummySCMsg]
                                                                   SCMsgSpawn (LaserObj l) -> [laser l]
                                                                   SCMsgInitialize p -> [observer p{playerName = playerNameStr}]
                                                                   _ -> [],
                                                  ooBounds = BoundingEmpty}
    let oo = processSCMsg changeMsg
    returnA -< oo {ooSpawnReq = ooSpawnReq oo ++ (case spawnWaterfall of
                                                      NoEvent -> []
                                                      Event () -> [terrainW])}

player :: Player -> SCMsg -> ObjectSF
player pl initMsg = switch (player' pl initMsg) (\(p,msg) -> player p msg)

player' :: Player -> SCMsg -> SF ObjInput (ObjOutput, Event (Player, SCMsg))
player' pl initMsg = proc ObjInput{oiGameInput=gi} -> do
    changeMsg <- loopPre initMsg detectChangeSF -< message gi
    let update = maybeEvent NoEvent (\(i,msg') -> case msg' of
                                                      SCMsgPlayer p -> if playerID pl == playerID p then Event p else NoEvent
                                                      _ -> NoEvent) changeMsg
        kill = maybeEvent NoEvent (\(i,msg') -> case msg' of
                                                    SCMsgSpawn (PlayerObj p) -> if playerID pl == playerID p then Event () else NoEvent
                                                    SCMsgRemove pID -> if playerID pl == pID then Event () else NoEvent
                                                    _ -> NoEvent) changeMsg
    pos <- (playerPos pl ^+^) ^<< integral -< playerVel pl
    let pl' = pl {playerPos = pos}
    returnA -< (ObjOutput {ooObsObjState = OOSPlayer pl {playerPos = pos},
                           ooNetworkMsgs = [],
                           ooKillReq = kill,
                           ooSpawnReq = maybeEvent [] (\_ -> ([killtext $ playerName pl] ++ (map particle $ generatePreloadedParticles pos))) kill,
                           ooBounds = let d = Vec3d (playerRadius pl', playerRadius pl', playerRadius pl')
                                      in BoundingBox (pos ^-^ d) (pos ^+^ d)}, fmap (\ev -> (ev,message gi)) update)

scoreboard :: ObjectSF
scoreboard = let addFrag pl ((p,s):rest) = if playerID pl == playerID p then (p,s+1):rest else (p,s) : addFrag pl rest
                 addFrag pl [] = [(pl,1)]
             in proc ObjInput{oiGameInput=gi} -> do
    changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    sb' <- loopPre (ScoreBoard{sbScores=[]}) 
                   (arr (\(chmsg,sb) -> dup $ maybeEvent sb (\(i,msg') -> case msg' of
                                                                              SCMsgFrag p -> sb {sbScores = addFrag p $ sbScores sb}
                                                                              _ -> sb) chmsg)) -< changeMsg
    returnA -< ObjOutput {ooObsObjState = OOSScoreBoard sb',
                          ooNetworkMsgs = [],
                          ooKillReq = NoEvent,
                          ooSpawnReq = [],
                          ooBounds = BoundingEmpty}

powerup :: PowerUp -> ObjectSF
powerup pow = proc ObjInput{oiColliding = collider} -> do
    kill <- after 15 () -< ()
    let collected = case collider of
                     Just (ObjOutput{ooObsObjState=oos}) ->
                        case oos of
                            OOSPlayer p -> True
                            OOSSelf p -> True
                            _ -> False
                     _ -> False
    returnA -< ObjOutput {ooObsObjState = OOSPowerUp pow,
                          ooNetworkMsgs = [],
                          ooKillReq = if collected then Event () else kill,
                          ooSpawnReq = [],
                          ooBounds = let d = Vec3d (powerupRadius pow, powerupRadius pow, powerupRadius pow)
                                     in BoundingBox (powerupPos pow ^-^ d) (powerupPos pow ^+^ d)}

laser :: Laser -> ObjectSF
laser l = proc (ObjInput {oiColliding = collider}) -> do
    p <- ((laserPos l) ^+^) ^<< integral -< laserVel l
    let l' = l {laserPos = p}
    kill <- repeatedly 3 () -< ()
    reflect' <- edge <<^ (/= Nothing) -< collider
    let ignore = case collider of
                     Just (ObjOutput{ooObsObjState=oos}) ->
                        case oos of
                            OOSPlayer p -> playerID p == laserpID l
                            OOSSelf p -> playerID p == laserpID l
                            OOSLaser _ -> True
                            _ -> False
                     _ -> False
        reflect = if ignore then NoEvent else reflect'
        newVel (Vec3d (x,y,z)) = if x >= y && x >= z then Vec3d (-x,y,z) else
                                 if y >= x && y >= z then Vec3d (x,-y,z) else Vec3d (x,y,-z)
        v' = newVel $ laserVel l
        l'' = l' {laserVel = v'}
    returnA -< ObjOutput {ooObsObjState = OOSLaser l',
                          ooNetworkMsgs = maybeEvent [] (\_ -> [(laserpID l, CSMsgKillLaser $ laserID l)]) kill ++
                                          maybeEvent [] (\_ -> [(laserpID l, CSMsgLaser l'')]) reflect,
                          ooKillReq = mergeBy (\_ _ -> ()) kill reflect,
                          ooSpawnReq = maybeEvent [] (\_ -> [laser l'']) reflect,
                          ooBounds = BoundingEmpty} --BoundingBox (p ^-^ Vec3d(laserRadf, laserRadf, 0)) (p ^+^ Vec3d(laserRadf, laserRadf, laserHeightf)) }


particle :: Particle -> ObjectSF
particle part = proc _ -> do
    let vel = particleVel part
    pos <- (particlePos part ^+^) ^<< integral -< 10 *^ vel
    let part' = part {particlePos = pos}
    kill <- repeatedly 3 () -< ()
    returnA -< ObjOutput {ooObsObjState = OOSParticle part',
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = [],
                          ooBounds = BoundingEmpty}

particle2 :: Particle -> ObjectSF
particle2 part = proc _ -> do
    pos <- (particlePos part ^+^) ^<< integral -< 10 *^ (unsafePerformIO $ randomTriple')
    let depth = (particleDepth part)
        part' = part {particlePos = pos, particleDepth = depth + 1}
        d10 = (fromIntegral depth) / 10.0
    spawn <- repeatedly 0.01 () -< ()
    kill <- repeatedly 0.3 () -< ()
    let spawnreq = if depth < 3 then maybeEvent [] (\_ -> [particle part']) spawn else []
    returnA -< ObjOutput {ooObsObjState = OOSParticle part',
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = spawnreq,
                          ooBounds = BoundingEmpty}

killtext :: String -> ObjectSF
killtext playerName = proc _ -> do
    kill <- repeatedly 3 () -< ()
    returnA -< ObjOutput {ooObsObjState = OOSKillText playerName,
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = [],
                          ooBounds = BoundingEmpty}


{-
particleSystem :: ParticleSystem -> ObjectSF
particleSystem partSys = proc _ -> do
--    pos <- (particlePos part ^+^) ^<< integral -< 10 *^ (unsafePerformIO $ randomTriple')

    let depth = 1
        partSys' = partSys -- {particlePos = pos, particleDepth = depth + 1}
    kill <- repeatedly 3 () -< ()
    returnA -< ObjOutput {ooObsObjState = OOSParticleSystem partSys',
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = [],
                          ooBounds = BoundingEmpty}
-}
player1 :: ID -> Vec3d -> ObjectSF
player1 pid p0 = proc (ObjInput {oiGameInput = gi}) -> do
    v <- arr $ maybe (Vec3d (0,0,0)) ((1 *^) . dirToVec) -< key gi
    p <- (p0 ^+^) ^<< integral -< v
    fireLaser <- edge -< leftClick gi
    changeVel <- loopPre (Vec3d (0,0,0)) (arr (\(vnew,vold) -> (if vnew == vold then NoEvent else Event (), vnew))) -< v
    let lsr = Laser {laserID = 1,
                     laserpID = pid,
                     laserPos = p,
                     laserVel = v,
                     laserStr = defLaserStr,
                     laserColor = Vec3d (0.5,0.5,0.5)}
        plyr = Player {playerID = pid,
                       playerPos = p,
                       playerVel = v,
                       playerAcc = Vec3d (0,0,0),
                       playerView = (0,0),
                       playerRadius = defRadius,
                       playerLife = maxLife,
                       playerEnergy = maxEnergy,
                       playerColor = Vec3d (0.5,0.5,0.5),
                       playerName = "i dunno what to call this thingy its useless anyway"}
    returnA -< ObjOutput {ooObsObjState = OOSPlayer (Player {playerID = pid,
                                                             playerPos = p,
                                                             playerVel = v,
                                                             playerAcc = Vec3d (0,0,0),
                                                             playerView = (0,0),
                                                             playerRadius = defRadius,
                                                             playerLife = maxLife,
                                                             playerEnergy = maxEnergy,
                                                             playerColor = Vec3d (0.5,0.5,0.5),
                                                             playerName = "player1"}),
                          ooNetworkMsgs = (\xs -> [x | Event x <- xs]) $
                                            fmap (\_ -> (pid,CSMsgLaser lsr)) fireLaser :
                                            fmap (\_ -> (pid,CSMsgPlayer plyr)) changeVel : [],
                          ooKillReq = NoEvent,
                          ooSpawnReq = maybeEvent [] (\_ -> [laser lsr]) fireLaser,
                          ooBounds = BoundingEmpty}
        where dirToVec (CharKey c) = case c of
                'w' -> Vec3d (0,0,1)
                's' -> Vec3d (0,0,-1)
                'a' -> Vec3d (0,1,0)
                'd' -> Vec3d (0,-1,0)
                'e' -> Vec3d (0,-1,1)
                'q' -> Vec3d (0,1,1)
                'z' -> Vec3d (0,1,-1)
                'c' -> Vec3d (0,-1,-1)
                _ -> Vec3d (0,0,0)
              dirToVec _ = Vec3d (0,0,0)
