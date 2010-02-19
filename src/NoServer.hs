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
import Vec3d

main :: IO ()
main = do
    glInit
    runGame "NoServer" Nothing (game [observer dummyPlayer, player dummyPlayer dummySCMsg, terrain0] >>> 
					 arr (\(ooss,msgs) -> (renderObsObjStates ooss, return ())))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())