{-# LANGUAGE Arrows #-}
{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   One of the most important modules                         *
*                  Define ObjectSF for the game, using Yampa                 *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
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
import Particles
import PowerUp

type ObjectSF = SF ObjInput ObjOutput

data ObjInput = ObjInput {
    oiGameInput :: !GameInput,
    oiColliding :: !(Maybe ObjOutput)
}

data ObjOutput = ObjOutput {
    ooObsObjState :: !ObsObjState,         -- Observable Object State -- for OpenGL rendering
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
renderObsObjState (OOSPowerUp pow) = renderPowerUp pow
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

    fwd   <- loopPre 0 $ arr $ setFromKey (CharKey 'W') -< gi
    bwd   <- loopPre 0 $ arr $ setFromKey (CharKey 'S') -< gi
    right <- loopPre 0 $ arr $ setFromKey (CharKey 'D') -< gi
    left  <- loopPre 0 $ arr $ setFromKey (CharKey 'A') -< gi
    keyWheel <- loopPre 0 $ arr $ checkMouseWheel -< gi
    stopEvent <- edge -< rightClick gi
    collideEvent <- edge <<^ (/= Nothing) -< collider

    let df = (fwd - bwd) *^ f
        dr = (right - left) *^ r
        du = keyWheel *^ up
        a = df ^+^ dr ^+^ du
        v = (if isEvent collideEvent then -4 else 1) *^ a --TODO: fix collision bug
    --v <- integral -< a
    p <- (playerPos pl ^+^) ^<< integral -< v

    let pow = case (collideEvent, collider) of
                  (Event (), Just (ObjOutput{ooObsObjState=OOSPowerUp pw})) -> Just pw
                  _ -> Nothing

        laserPowEvent = maybe NoEvent (\pow -> case powerupType pow of
                                                   StrengthenLaser str -> Event str
                                                   _ -> NoEvent) pow
        radiusPowEvent = maybe NoEvent (\pow -> case powerupType pow of
                                                    DecreaseRadius r -> Event (r * playerRadius pl)
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

    --changeVel <- tagWith () ^<< loopPre (Vec3d (0,0,0)) detectChangeSF -< v
    changeVel <- edgeBy (\prev cur -> if prev /= cur then Just () else Nothing) zeroVector -< v
    --changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
    let hitEvent = case changeMsg of
                       Event (_,SCMsgHit h) -> if playerID pl == player2ID h then changeMsg else NoEvent
                       _ -> NoEvent

    life <- loopPre (playerLife pl) (arr (\(hev,life) -> dup $ event life (\(_,SCMsgHit h) -> life - hitStr h) hev)) -< hitEvent
    let killerHit = case hitEvent of
                       Event (_,SCMsgHit h) -> Just h
                       _ -> Nothing
        kill = if life <= 0 then Event (fromJust killerHit) else NoEvent
        pl' = pl {playerPos = p,
                  playerVel = v,
                  playerAcc = a,
                  playerView = (theta,phi),
                  playerLife = life,
                  playerRadius = radius}
    returnA -< ObjOutput {ooObsObjState = OOSSelf pl',
                          ooNetworkMsgs = (\xs -> [x | Event x <- xs]) $
                                            [fireLaser `tag` (playerID pl, CSMsgLaser lsr),
                                             foldl (mergeBy const) NoEvent [changeVel, hitEvent `tag` ()] `tag` (playerID pl, CSMsgPlayer pl'),
                                             fmap (\hit -> (playerID pl, CSMsgDeath hit)) kill],
                          ooKillReq = kill `tag` (),
                          ooSpawnReq = event [] (\_ -> [laser lsr]) fireLaser,
                          ooBounds = let v = Vec3d (playerRadius pl', playerRadius pl', playerRadius pl')
                                     in BoundingBox (p ^-^ v) (p ^+^ v)}

serverObject :: RandomGen g => g -> String -> ObjectSF
serverObject g playerNameStr = proc (ObjInput {oiGameInput = gi}) -> do
    spawnWaterfall <- edge <<^ (\gi -> key gi == Just (CharKey 'L')) -< gi
    spawnPowerup <- edge <<^ (\gi -> key gi == Just (CharKey 'P')) -< gi
    spawnPeriodically <- repeatedly 20 () -< gi
    --changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
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
                                                      Event () -> [terrainW])
                                              ++ (case spawnPowerup `lMerge` spawnPeriodically of
                                                      NoEvent -> []
                                                      Event () -> [powerup $ genPow 3])
                                                      }

player :: Player -> SCMsg -> ObjectSF
player pl initMsg = switch (player' pl initMsg) (\(p,msg) -> player p msg)

player' :: Player -> SCMsg -> SF ObjInput (ObjOutput, Event (Player, SCMsg))
player' pl initMsg = proc ObjInput{oiGameInput=gi} -> do
    --changeMsg <- loopPre initMsg detectChangeSF -< message gi
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) initMsg -< message gi
    let update = event NoEvent (\(i,msg') -> case msg' of
                                                      SCMsgPlayer p -> if playerID pl == playerID p then Event p else NoEvent
                                                      _ -> NoEvent) changeMsg
        kill = event NoEvent (\(i,msg') -> case msg' of
                                                    SCMsgSpawn (PlayerObj p) -> if playerID pl == playerID p then Event () else NoEvent
                                                    _ -> NoEvent) changeMsg
        exit = event NoEvent (\(i,msg') -> case msg' of
                                                    SCMsgRemove pID -> if playerID pl == pID then Event() else NoEvent
                                                    _ -> NoEvent) changeMsg
    pos <- (playerPos pl ^+^) ^<< integral -< playerVel pl
    let pl' = pl {playerPos = pos}
        rad = playerRadius pl
    returnA -< (ObjOutput {ooObsObjState = OOSPlayer pl', -- {playerPos = pos},
                           ooNetworkMsgs = [],
                           ooKillReq = kill `lMerge` exit, -- left-biased merge
                           ooSpawnReq = event [] (\_ -> (map particle $ generatePreloadedParticles pos)) kill,
                           ooBounds = let d = Vec3d (rad, rad, rad)
                                      in BoundingBox (pos ^-^ d) (pos ^+^ d)},
                fmap (\ev -> (ev,message gi)) update)

scoreboard :: ObjectSF
scoreboard = let addFrag pl ((p,s):rest) = if playerID pl == playerID p then (p,s+1):rest else (p,s) : addFrag pl rest
                 addFrag pl [] = [(pl,1)]
             in proc ObjInput{oiGameInput=gi} -> do
    --changeMsg <- loopPre dummySCMsg detectChangeSF -< message gi
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
    let killAnnounce = event NoEvent (\(i, msg') -> case msg' of
                                                     SCMsgFrag killer killed -> Event (playerName killer ++ " just killed " ++ playerName killed)
                                                     _ -> NoEvent) changeMsg
    sb' <- loopPre (ScoreBoard{sbScores=[]})
                   (arr (\(chmsg,sb) -> dup $ event sb (\(i,msg') -> case msg' of
                                                                      SCMsgFrag killer killed -> sb {sbScores = addFrag killer $ sbScores sb}
                                                                      _ -> sb) chmsg)) -< changeMsg
    returnA -< ObjOutput {ooObsObjState = OOSScoreBoard sb',
                          ooNetworkMsgs = [],
                          ooKillReq = NoEvent,
                          ooSpawnReq = event [] (\txt -> [killtext txt]) killAnnounce,
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
    newTheta <- ((fst.powerupView $ pow) ^+^) ^<< integral -< 0.4 -- spinning horizontally
    let oldPhi = snd.powerupView $ pow
    returnA -< ObjOutput {ooObsObjState = OOSPowerUp pow{powerupView=(newTheta, oldPhi)},
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
                          ooNetworkMsgs = event [] (\_ -> [(laserpID l, CSMsgKillLaser $ laserID l)]) kill ++
                                          event [] (\_ -> [(laserpID l, CSMsgLaser l'')]) reflect,
                          ooKillReq = mergeBy (\_ _ -> ()) kill reflect,
                          ooSpawnReq = event [] (\_ -> [laser l'']) reflect,
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
{-
particle2 :: Particle -> ObjectSF
particle2 part = proc _ -> do
    pos <- (particlePos part ^+^) ^<< integral -< 10 *^ (unsafePerformIO $ randomTriple')
    let depth = (particleDepth part)
        part' = part {particlePos = pos, particleDepth = depth + 1}
        d10 = (fromIntegral depth) / 10.0
    spawn <- repeatedly 0.01 () -< ()
    kill <- repeatedly 0.3 () -< ()
    let spawnreq = if depth < 3 then event [] (\_ -> [particle part']) spawn else []
    returnA -< ObjOutput {ooObsObjState = OOSParticle part',
                          ooNetworkMsgs = [],
                          ooKillReq = kill,
                          ooSpawnReq = spawnreq,
                          ooBounds = BoundingEmpty}
-}

-- TODO: Change to something like perishable text, for general text display of 3 seconds
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
                          ooSpawnReq = event [] (\_ -> [laser lsr]) fireLaser,
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

