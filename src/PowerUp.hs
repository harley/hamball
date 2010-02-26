{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Rendering code for powerups, fun part of the game         *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module PowerUp where

import Common
import FRP.Yampa
import Vec3d
import Graphics.Rendering.OpenGL as OpenGL
import TerrainData
import Render
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL.GL.CoordTrans
import System.Random
import System.IO.Unsafe (unsafePerformIO)

spawnLocations :: [Position3]
spawnLocations = [Vec3d (50, 0, 0), Vec3d (60,10,0), Vec3d (50,100,100), Vec3d(30,(-60), 10), Vec3d (20,(-75), 35)]

strengthPow, xPow, tinyPow :: PowerUp
strengthPow = PowerUp (spawnLocations !! (unsafePerformIO $ getRandom 5)) 4 (StrengthenLaser 4) zeroVector
xPow = PowerUp (spawnLocations !! (unsafePerformIO $ getRandom 5)) 2 XRayVision zeroVector
tinyPow = PowerUp (spawnLocations !! (unsafePerformIO $ getRandom 5)) 3 (DecreaseRadius 3) zeroVector
-- TODO: add power up to refill life

powList :: [PowerUp]
powList = [strengthPow, tinyPow, xPow]

getRandom :: Int -> IO(Int)
getRandom n = do
  r <- newStdGen
  let (x, _) = next r
  return $ x `mod` n

genPow :: Int -> PowerUp
genPow n = powList !! (unsafePerformIO $ getRandom n)

renderPowerUp :: PowerUp -> IO ()
renderPowerUp p = preservingMatrix $ do
    loadIdentity
    translate $ vector3 $ powerupPos p
    --materialEmission FrontAndBack $= colorf Blue
    let r = double (powerupRadius p) / (sqrt 2)
    blend $= Enabled
    blendFunc $= (One, SrcColor)
    let
        radianToDegrees x = 180 * x / pi
        --style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside LineStyle
    preservingMatrix $ do
        rotate (radianToDegrees $ fst $ powerupView p) (vector3 $ Vec3d(0, 0, 1))
        rotate (radianToDegrees $ snd $ powerupView p) (vector3 $ Vec3d(0, 1, 0))
        renderQuad powerupTexture (Vertex3 (0) r (-r)) (Vertex3 (0) (r) r) (Vertex3 0 (-r) r) (Vertex3 0 (-r) (-r))


    -- TODO: Fix this billboard-style name tag overhead
    let offset =  float (length $ show p) / 2.0
    preservingMatrix $ do
        loadIdentity
        --let v = Vec3d(Matrix[8], Matrix[9], -Matrix[10]);

        translate (vector3 (powerupPos p ^+^ 1.1 *^ Vec3d(offset * 0.2,offset * 0.2,powerupRadius p)))
        OpenGL.scale 0.05 0.05 (0.05 :: GLdouble)
        rotate (-90) (vector3 $ Vec3d(0,0,1))
        rotate 90 (vector3 $ Vec3d(1,0,0))
        -- Undo Rotations
        -- Redo Scalings
        renderString Fixed8x16 (show p)
        -- Use withMatrix... or access elements directly
        blend $= Disabled
        --multMatrix newMT

