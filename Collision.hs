module Collision where
import FRP.Yampa
import Vec3d
import Common
import List
import Terrain

-- Currently assuming equal mass (variable radius should be okay).
-- Collisions are fully elastic.
{-
collisionPP :: (Player, Player) -> Maybe (Player, Player)
collisionPP (player1,player2) =
    let separation = playerPos player2 ^-^ playerPos player1
     in if playerRadius player1 + playerRadius player2 < (len $ debug ("Sep: " ++ (show $ len separation) ++ ", Rad = " ++ (show $ playerRadius player1)) $ separation)
        then Nothing else
         let dir1 = normalize $ separation
             dir2 = negateVector dir1
             (v1,v2) = (playerVel player1, playerVel player2)
             (v1a,v2a) = (scale dir1 (dir1 `dot` v1), scale dir2 (dir2 `dot` v2))
             (v1n,v2n) = (v1 ^-^ v1a, v2 ^-^ v2a)
             (newP1, newP2) = (player1{playerPos=v1a ^+^ v1n}, player2{playerPos= v2a ^+^ v2n})
          in debug ("Bah") Just -- $ debug ("Sep...collision occured btw " ++ (show $ playerID newP1) ++ " && " ++ (show $ playerID newP2))
                (newP1, newP2)
-}
collisionLP :: (Laser, Laser, Player) -> Maybe Hit
collisionLP (lprev, l, p) =
    let p1 = laserPos lprev
        p2 = laserPos l
        p3 = playerPos p
        seg = p2 ^-^ p1
        u = ((p3 ^-^ p1) .* seg) / (seg .* seg)
        ipt = p1 ^+^ (u *^ seg)
    in if 0 <= u && u < 1 && len (p3 ^-^ ipt) < playerRadius p
       then Just $ Hit{player1ID=laserpID l, player2ID=playerID p, hitLaserID=laserID l, hitStr=laserStr l}
       else Nothing

--collisionPT :: (Player, TerrainElement) -> Maybe Vec3d
--collisionPT (p, SimpleTerrain (Quad p1' p2' p3' p4' (Transform offset scl theta phi)) _ _) =
--    let appT = (offset +).(scl *^).id.id -- assuming no rotated quads
--        (p1,p2,p3,p4) = (appT p1', appT p2', appT p3', appT p4')
--     in Nothing
