module Laser where

import Common
import FRP.Yampa as Yampa
import Vec3d
import GameInput
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.String
import Control.Monad
import Colors

laserRad :: GLdouble
laserRad = 0.5

laserHeight :: GLdouble
laserHeight = 10

laserRadf, laserHeightf :: Float
laserRadf = 0.5
laserHeightf = 10

renderLaser :: Laser -> IO ()
renderLaser l = do
    loadIdentity
    preservingMatrix $ do
        translate $ vector3 $ laserPos l -- Move to cylinder center
        let drawDir = Vec3d(0,0,1)
            dir = Yampa.normalize (laserVel l)
            rotAxis =  drawDir `cross` dir
            rotAngle = acos(dir .* drawDir) / pi * 180

        preservingMatrix $ do
            when (rotAxis /= zeroVector) $
                rotate rotAngle $ vector3 rotAxis
            materialEmission FrontAndBack $= vecToColor (laserColor l) -- make it Emission so that it's not affected by light
            materialDiffuse FrontAndBack $= colorf Black
            let style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle
            renderQuadric style $ Cylinder 0.0 laserRad laserHeight 10 10

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
                        (p6,s6) = untildelim $ drop 1 s5
                     in Laser {laserID = read p1,
                               laserpID = read p2,
                               laserPos = readVec3d p3,
                               laserVel = readVec3d p4,
                               laserStr = read p5,
                               laserColor = readVec3d p6}
