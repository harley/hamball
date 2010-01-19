module Projection where

import Foreign.C

import Common
import Vec3d

{-# INCLUDE "projection.c" #-}
foreign import ccall unsafe "reproject" reproject :: CInt -> CInt

-- n=90 yields 1 pix per deg^2
angles3d :: Integer -> [(Float,Float)]
angles3d n = [(theta, phi) | phi <- phis, theta <- thetas]
    where angle k x = 0.5 * pi * fromInteger x / fromInteger k
          thetas = map (angle n) [(-2*n)..(2*n-1)]
          phis = map (angle n) [(-1*n)..(n-1)]

xyz :: (Float, Float) -> Vec3d
xyz (theta, phi) = Vec3d (cos theta * cos phi, sin theta * cos phi, sin phi)

projectToCube v@(Vec3d(x,y,z)) =
    case (maxIndex v, signum x, signum y, signum z) of
        (1,s,_,_) -> (scale v (s/x),   s)
        (2,_,s,_) -> (scale v (s/y), 2*s)
        (3,_,_,s) -> (scale v (s/z), 3*s)
    where maxIndex (Vec3d(a,b,c)) = if abs a >= abs b && abs a >= abs c then 1 
                                    else if abs b >= abs c then 2 else 3

coordsOnCube n = map (projectToCube.xyz) $ angles3d n

-- negative x is forwards; everything else is situtated to be the simplest
-- rotation from that direction
mercatorMap n w =
    let c = coordsOnCube n
        f (a,b,c) = (a, round $ fromInteger (w-1) * b, round $ fromInteger (w-1) * c)
        coord (Vec3d (x,y,z),(-1)) = f (-1,y,z)
        coord (Vec3d (x,y,z),  1 ) = f ( 1,-1*y,z)
        coord (Vec3d (x,y,z),(-2)) = f (-2,-1*x,z)
        coord (Vec3d (x,y,z),  2 ) = f ( 2,x,z)
        coord (Vec3d (x,y,z),(-3)) = f (-3,x,y)
        coord (Vec3d (x,y,z),  3 ) = f ( 3,x,-1*y)
     in map coord c
