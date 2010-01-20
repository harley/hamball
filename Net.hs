module Net where

import Network
import System.IO
import FRP.Yampa
import Common
import GameInput
import Player
import Laser
import Monad

instance Stringifiable Hit where

    stringify h = (show $ player1ID h) ++ delim ++
                  (show $ player2ID h) ++ delim ++
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

instance Stringifiable SCMsg' where

    stringify (SCMsgInitialize p) = "initialize:" ++ (stringify p)
    stringify (SCMsgPlayer p) = "player:" ++ (stringify p)
    stringify (SCMsgHit h) = "hit:" ++ (stringify h)
    stringify (SCMsgSpawn obj) = "spawn:" ++ (stringify obj)
    stringify (SCMsgFrag p) = "frag:" ++ (stringify p)

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "initialize" -> SCMsgInitialize $ destringify $ drop 1 s1
                          "player"     -> SCMsgPlayer $ destringify $ drop 1 s1
                          "hit"        -> SCMsgHit $ destringify $ drop 1 s1
                          "spawn"      -> SCMsgSpawn $ destringify $ drop 1 s1
                          "frag"       -> SCMsgFrag $ destringify $ drop 1 s1

instance Stringifiable CSMsg' where

    stringify (CSMsgPlayer p) = "player:" ++ (stringify p)
    stringify (CSMsgUpdate p) = "update:" ++ (stringify p)
    stringify (CSMsgLaser l) = "laser:" ++ (stringify l)
    stringify (CSMsgKillLaser ident) = "killlaser:" ++ (show ident)
    stringify (CSMsgDeath ident) = "death:" ++ (show ident)
    stringify (CSMsgJoin name) = "join:" ++ name
    stringify (CSMsgExit) = "exit"

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "player" -> CSMsgPlayer $ destringify $ drop 1 s1
                          "update" -> CSMsgUpdate $ destringify $ drop 1 s1
                          "laser" -> CSMsgLaser $ destringify $ drop 1 s1
                          "killlaser" -> CSMsgKillLaser $ read $ drop 1 s1
                          "death" -> CSMsgDeath $ read $ drop 1 s1
                          "join" -> CSMsgJoin $ drop 1 s1
                          "exit" -> CSMsgExit

instance Stringifiable SCMsg where

    stringify (ident,scmsg') = (show ident) ++ ":" ++ (stringify scmsg')
    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)

instance Stringifiable CSMsg where

    stringify (ident,csmsg') = (show ident) ++ ":" ++ (stringify csmsg')

    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)

fetchSCMsg :: ReactChan GameInput -> Handle -> IO ()
fetchSCMsg rch h = do
    ln <- hGetLine h
    let scMsg = destringify ln :: SCMsg
        b = case scMsg of
                (_,SCMsgHit h) -> True
                _ -> False
    react rch (\gi -> gi {message = destringify ln}) False

-- Send msg from Client to Server
sendCSMsg :: Handle -> CSMsg -> IO ()
sendCSMsg h msg = do
    hPutStrLn h (stringify msg)
    hFlush h -- flushing is required here.

