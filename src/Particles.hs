{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Rendering code for explosive particles when a player dies *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module Particles where
import Common
import Vec3d
import System.IO.Unsafe
import System.Random
import Graphics.Rendering.OpenGL
import TerrainData

-- Number of particles
particleNum :: Int
particleNum = 100

-- Render particles
renderParticle :: Particle -> IO ()
renderParticle p = do
    preservingMatrix $ do
        loadIdentity
        texture Texture2D $= Enabled
        textureFunction $= Decal
        textureBinding Texture2D $= bloodTexture
        translate $ vector3 $ particlePos p
        -- materialEmission FrontAndBack $= colorf Colors.Blue
        renderQuadric (QuadricStyle (Just Smooth) GenerateTextureCoordinates Outside FillStyle) (Sphere 0.15 2 2)
        texture Texture2D $= Disabled

renderParticleSystem :: ParticleSystem -> IO()
renderParticleSystem ps = do
  print "Rendering particle system"

-- RANDOM STUFF
-- From http://en.wikibooks.org/wiki/Haskell/Hierarchical_libraries/Randoms

randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

randomSeed' :: IO(Int)
randomSeed' = do
  r <- newStdGen
  let (x, _) = next r
  return x

randomTriple' :: IO(Vec3d)
randomTriple' = do
  r <- newStdGen

  let (x1, r2) = randomR (-1.0,1.0) r
      (x2, r3) = randomR (-1.0,1.0) r2
      (x3, _) = randomR (-1.0,1.0) r3
      result = Vec3d(x1,x2,x3)
  return result

-- UNSAFE, UNSAFE!!!!!!!!
tlist :: [Float]
tlist = map (\x->(0.5-x)) $ take (3*particleNum) $ (randomList (unsafePerformIO randomSeed')::[Float])

preloadedRandomVecs :: [Vec3d]
preloadedRandomVecs = f tlist
  where f (x:y:z:r) = Vec3d(x,y,z):f(r)
        f _ = []

generatePreloadedParticles :: Vec3d -> [Particle]
generatePreloadedParticles pos = map (\v-> Particle pos v 1.0 0) preloadedRandomVecs

generatePreloadedParticles2 :: Vec3d -> [Particle]
generatePreloadedParticles2 pos = map (\v-> Particle pos (unsafePerformIO $ randomTriple') 1.0 0) preloadedRandomVecs

