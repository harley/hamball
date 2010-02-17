{-# LANGUAGE TypeSynonymInstances, Arrows #-}
module Net where

import System.IO
import FRP.Yampa
import Common
import GameInput
import Vec3d


instance Stringifiable Laser where

    stringify l = (show $ laserID l) ++ delim ++
                  (show $ laserpID l) ++ delim ++
                  (showVec3d $ laserPos l) ++ delim ++
                  (showVec3d $ laserVel l) ++ delim ++
                  (show $ laserStr l) ++ delim ++
                  (showVec3d $ laserColor l)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                        (p4,s4) = untildelim $ drop 1 s3
                        (p5,s5) = untildelim $ drop 1 s4
                        (p6,_) = untildelim $ drop 1 s5
                     in Laser {laserID = read p1,
                               laserpID = read p2,
                               laserPos = readVec3d p3,
                               laserVel = readVec3d p4,
                               laserStr = read p5,
                               laserColor = readVec3d p6}

instance Stringifiable Hit where

    stringify h = (show $ player1ID h)  ++ delim ++
                  (show $ player2ID h)  ++ delim ++
                  (show $ hitLaserID h) ++ delim ++
                  (show $ hitStr h)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                     in Hit {player1ID = read p1,
                             player2ID = read p2,
                             hitLaserID = read p3,
                             hitStr = read $ drop 1 s3}

instance Stringifiable Obj where

    stringify (PlayerObj p) = "player:" ++ (stringify p)
    stringify (LaserObj l) = "laser:" ++ (stringify l)

    destringify s = let (p1,s1) = span (/= ':') s
                    in case p1 of
                          "player" -> PlayerObj $ destringify $ drop 1 s1
                          "laser" -> LaserObj $ destringify $ drop 1 s1
                          _ -> error $ "Bad msg: can only be player or laser, but is " ++ p1

instance Stringifiable SCMsg' where

    stringify (SCMsgInitialize p) = "initialize:" ++ (stringify p)
    stringify (SCMsgPlayer p) = "player:" ++ (stringify p)
    stringify (SCMsgHit h) = "hit:" ++ (stringify h)
    stringify (SCMsgSpawn obj) = "spawn:" ++ (stringify obj)
    stringify (SCMsgFrag finalHit) = "frag:" ++ (stringify finalHit)
    stringify (SCMsgRemove pID)   = "remove:" ++ (show pID)

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "initialize" -> SCMsgInitialize $ destringify $ drop 1 s1
                          "player"     -> SCMsgPlayer $ destringify $ drop 1 s1
                          "hit"        -> SCMsgHit $ destringify $ drop 1 s1
                          "spawn"      -> SCMsgSpawn $ destringify $ drop 1 s1
                          "frag"       -> SCMsgFrag $ destringify $ drop 1 s1
                          "remove"     -> SCMsgRemove $ read $ drop 1 s1
                          _ -> error $ "Bad msg format for SCMsg': " ++ p1

instance Stringifiable CSMsg' where

    stringify (CSMsgPlayer p) = "player:" ++ (stringify p)
    stringify (CSMsgUpdate p) = "update:" ++ (stringify p)
    stringify (CSMsgLaser l) = "laser:" ++ (stringify l)
    stringify (CSMsgKillLaser ident) = "killlaser:" ++ (show ident)
    stringify (CSMsgDeath h) = "death:" ++ (stringify h)
    stringify (CSMsgJoin name) = "join:" ++ name
    stringify (CSMsgExit name) = "exit:" ++ name

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "player" -> CSMsgPlayer $ destringify $ drop 1 s1
                          "update" -> CSMsgUpdate $ destringify $ drop 1 s1
                          "laser" -> CSMsgLaser $ destringify $ drop 1 s1
                          "killlaser" -> CSMsgKillLaser $ read $ drop 1 s1 -- TODO: read here is a bit unsafe
                          "death"-> CSMsgDeath $ destringify $ drop 1 s1
                          "join" -> CSMsgJoin $ drop 1 s1
                          "exit" -> CSMsgExit $ drop 1 s1
                          _ -> error $ "Bad msg format for CSMsg': " ++ p1

instance Stringifiable SCMsg where

    stringify (ident,scmsg') = (show ident) ++ ":" ++ (stringify scmsg')
    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)

instance Stringifiable CSMsg where

    stringify (ident,csmsg') = (show ident) ++ ":" ++ (stringify csmsg')

    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)


instance Stringifiable Player where

    stringify p = (show $ playerID p) ++ delim ++
                  (showVec3d $ playerPos p) ++ delim ++
                  (showVec3d $ playerVel p) ++ delim ++
                  (showVec3d $ playerAcc p) ++ delim ++
                  (show $ playerView p) ++ delim ++
                  (show $ playerRadius p) ++ delim ++
                  (show $ playerLife p) ++ delim ++
                  (show $ playerEnergy p) ++ delim ++
                  (showVec3d $ playerColor p) ++ delim ++
                  (show $ playerName p)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                        (p4,s4) = untildelim $ drop 1 s3
                        (p5,s5) = untildelim $ drop 1 s4
                        (p6,s6) = untildelim $ drop 1 s5
                        (p7,s7) = untildelim $ drop 1 s6
                        (p8,s8) = untildelim $ drop 1 s7
                        (p9,s9) = untildelim $ drop 1 s8
                        (p10,_) = untildelim $ drop 1 s9
                    in Player {playerID = read p1,
                               playerPos = readVec3d p2,
                               playerVel = readVec3d p3,
                               playerAcc = readVec3d p4,
                               playerView = read p5,
                               playerRadius = read p6,
                               playerLife = read p7,
                               playerEnergy = read p8,
                               playerColor = readVec3d p9,
                               playerName = read p10}




fetchSCMsg :: ReactChan GameInput -> Handle -> IO ()
fetchSCMsg rch h = do
    ln <- hGetLine h
    printFlush ln -- for debug
    reactWriteChan rch (\gi -> gi {message = destringify ln}) False

-- Send msg from Client to Server
sendCSMsg :: Handle -> CSMsg -> IO ()
sendCSMsg h msg = do
    hPutStrLn h (stringify msg)
    hFlush h -- required.

