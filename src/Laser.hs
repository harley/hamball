{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Rendering code for lasers                                 *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module Laser where

import Common
import FRP.Yampa as Yampa
import Vec3d
import Graphics.Rendering.OpenGL
import Control.Monad
import Colors

laserRad, laserHeight :: GLdouble
laserRad = 0.5
laserHeight = 10

laserRadf, laserHeightf :: Float
laserRadf = float laserRad
laserHeightf = float laserHeight

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

