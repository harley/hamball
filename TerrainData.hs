module TerrainData where

import Vec3d
import Graphics.Rendering.OpenGL hiding (Texture)
import GHC.Float
import Colors
import Terrain
import Textures
import Data.Maybe
import System.IO.Unsafe
import TextureFonts
import Data.Complex

-------------------------------------------------------------------------------------------------------------
-- Mandelfall aka Waterbrot
--      by Alex

dr, mdepth, msize, mxoffset, myoffset :: Float
dr = 0.1
mdepth = 48
msize = 23
mxoffset = -2.3
myoffset = -2.3

mkcol :: GLfloat -> Surface
mkcol x = let r = x-0.75
              g = 0.25
              b = 0.25+x
              a = 1-x
           in Terrain.Color (Col {cdiffuse=(Color4 r g b a),
                                  cspecular=(Color4 r g b a),
                                  cambient=(Color4 r g b a),
                                  cemissive=(Color4 r g b a),
                                  cshininess=100})

mand :: Float -> Float -> Float
mand x y = mand' (x:+y) 0 0
mand' :: Complex Float -> Complex Float -> Float -> Float
mand' xy z i = if magnitude z > 2 then i
               else if i >= dr*mdepth then dr*mdepth
               else mand' xy (z*z+xy) (i + dr)

surfaceSegment :: (Float -> Float -> Float) -> Float -> Float -> TerrainElement
surfaceSegment f x y = CompoundTerrain nullTransform $
    let f00 = f x y
        f10 = f (x+dr) y
        f01 = f x (y+dr)
        f11 = f (x+dr) (y+dr)
     in [SimpleTerrain (Tri (Vec3d (x,y,f00)) (Vec3d (x+dr,y,f10)) (Vec3d (x,y+dr,f01)) nullTransform)
                       (mkcol $ (f00 + f10 + f01) / (3*mdepth*dr)),
         SimpleTerrain (Tri (Vec3d (x+dr,y+dr,f11)) (Vec3d (x,y+dr,f01)) (Vec3d (x+dr,y,f10)) nullTransform)
                       (mkcol $ (f11 + f10 + f01) / (3*mdepth*dr))]

waterfall :: TerrainElement
waterfall = CompoundTerrain nullTransform{toffset=Vec3d(76,-15,53),tscale=Vec3d(4,4,-15),ttheta=90}
    [surfaceSegment mand x y | x <- map ((mxoffset +).(dr *)) [0..msize-1], y <- map ((myoffset +).(dr *)) [0..msize-1]]


--------------------------------------------------------------------------------------------------------------
-- Some transforms for testing

nullTransform :: Transform
nullTransform = Transform{toffset=(Vec3d(0.0,0.0,0.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

testTransform :: Transform
testTransform = Transform{toffset=(Vec3d(5.0,1.0,0.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=90.0, tphi=90.0}

moveBack :: Transform
moveBack = Transform (Vec3d(20, 0,0)) (Vec3d(1,1,1)) 0 0
{-
glutObjectExample :: Geometry
glutObjectExample = GLUTObject (Teapot 0.5) nullTransform
-}
quadExample :: Geometry
quadExample = Quad (Vec3d(9,-1,-1)) (Vec3d(9,-1,1)) (Vec3d(9,1,1)) (Vec3d(9,1,-1)) nullTransform

quad0 :: TerrainElement
quad0 = SimpleTerrain quadExample surfaceExample

floorDepth :: Float
floorDepth = -20
floorExample :: Geometry
floorExample = Quad (Vec3d(0,-100,floorDepth)) (Vec3d(100,-100,floorDepth)) (Vec3d(100,100,floorDepth)) (Vec3d(0,100,floorDepth)) nullTransform
{-
vertex $ (Vertex3 ((5)::GLfloat) (-1) (-1))
        vertex $ (Vertex3 ((5)::GLfloat) (-1) 1)
        vertex $ (Vertex3 ((5)::GLfloat) 1 (1))
        vertex $ (Vertex3 (5::GLfloat) 1 (-1))
-}

surfaceExample :: Surface
surfaceExample = Terrain.Color (Col {cdiffuse=(Color4 0.5 0.0 0.0 1.0),cspecular=(Color4 0.2 0.2 0.0 1.0),cambient=(Color4 0.0 0.0 0.0 0.0),cemissive=(Color4 1.0 0.0 0.0 1.0),cshininess=100})

--
makeTextureUnsafe :: String -> Surface
makeTextureUnsafe str = Terrain.Texture (fromJust $ unsafePerformIO (getAndCreateTexture str))


--- BAD ---
akwTexture :: Surface
akwTexture = makeTextureUnsafe "bricks"

bectonTexture :: Surface
bectonTexture = makeTextureUnsafe "stone_small"

floorTexture :: Surface
floorTexture = makeTextureUnsafe "concrete"

skyTexture :: Surface
skyTexture = makeTextureUnsafe "blue_sky3"

hamsterTexture :: Maybe TextureObject
hamsterTexture = unsafePerformIO (getAndCreateTexture "hamTex")

explodeTexture :: Maybe TextureObject
explodeTexture = unsafePerformIO (getAndCreateTexture "explode_texture")

crosshairTexture :: Maybe TextureObject
crosshairTexture = unsafePerformIO (getAndCreateTexture "crosshaira")

bloodTexture :: Maybe TextureObject
bloodTexture = unsafePerformIO (getAndCreateTexture "blood_texture")

glowTexture :: Maybe TextureObject
glowTexture = unsafePerformIO (getAndCreateTexture "glow_texture")

tex :: Maybe TextureObject
base :: DisplayList
(tex,base) = unsafePerformIO buildFonts
--numbase = unsafePerformIO buildBigNums -- load gigantic numbers to display scores or sth

--------------------------------------------------------------------------------------------------------------
-- Demo Terrain

surfacef :: Colors.Color -> Surface
surfacef x = Terrain.Color (Col {cdiffuse=(colorf x),cspecular=(Color4 0.0 0.0 0.0 1.0),cambient=(colorf x),cemissive=(Color4 0.0 0.0 0.0 1.0),cshininess=50})


mt :: Vec3d -> Transform
mt x = Transform{toffset=x, tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

pillarColor :: Surface
pillarColor = Terrain.Color (Col {cdiffuse=(Color4 0.7 0.7 0.7 1.0),cspecular=(Color4 0.1 0.1 0.1 1.0),cambient=(Color4 1.0 1.0 1.0 1.0),cemissive=(Color4 0.0 0.0 0.0 1.0),cshininess=100})

buildingColor :: Surface
buildingColor = Terrain.Color (Col {cdiffuse=(Color4 0.5 0.0 0.0 1.0),cspecular=(Color4 0.2 0.2 0.0 1.0),cambient=(Color4 0.0 0.0 0.0 0.0),cemissive=(Color4 1.0 0.0 0.0 1.0),cshininess=100})

boxGeometry :: Geometry
boxGeometry = Cube 1.0 nullTransform

cylinderGeometry :: Geometry
cylinderGeometry = GLUQuadric (Cylinder 1.0 1.0 1.0 10 10) (QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle) nullTransform

bectonBottom :: TerrainElement
bectonBottom = CompoundTerrain Transform{toffset=(Vec3d(3.0,0.0,1.5)), tscale=(Vec3d(7.0,1.0,3.0)), ttheta=0.0, tphi=0.0}
  [SimpleTerrain boxGeometry bectonTexture]

bectonTop :: TerrainElement
bectonTop = CompoundTerrain Transform{toffset=(Vec3d(3.0,2.0,2.0)), tscale=(Vec3d(7.0,3.0,2.0)), ttheta=0.0, tphi=0.0}
  [SimpleTerrain boxGeometry bectonTexture]


pillar :: TerrainElement
pillar = CompoundTerrain Transform{toffset=(Vec3d(0.0,0.0,0.0)), tscale=(Vec3d(0.25,0.25,1.0)), ttheta=0.0, tphi=0.0}
  [SimpleTerrain cylinderGeometry (surfacef Grey)]

pillars :: TerrainElement
pillars = CompoundTerrain Transform{toffset=(Vec3d(0.0,1.5,0.0)), tscale=(Vec3d(1,1,1.0)), ttheta=0.0, tphi=0.0}
  [CompoundTerrain (mt $ Vec3d(0.5,1.0,0.0)) [pillar],
  CompoundTerrain (mt $ Vec3d(1.5,1.0,0.0)) [pillar],
  CompoundTerrain (mt $ Vec3d(2.5,1.0,0.0)) [pillar],
  CompoundTerrain (mt $ Vec3d(3.5,1.0,0.0)) [pillar],
  CompoundTerrain (mt $ Vec3d(4.5,1.0,0.0)) [pillar],
  CompoundTerrain (mt $ Vec3d(5.5,1.0,0.0)) [pillar]]

tunnel :: TerrainElement
tunnel = CompoundTerrain Transform{toffset=Vec3d(8,0,1),tscale=Vec3d(10,0.5,0.5), ttheta=0, tphi=0}
    [SimpleTerrain boxGeometry floorTexture]
--}
akw, akw1 :: TerrainElement
--akw = SimpleTerrain (GLUTObject (Cube 1.0) trans) akwTexture Solid
--    where trans = Transform{toffset = Vec3d (11,0,1), tscale = Vec3d (3,4,2), ttheta = 0, tphi = 0}
akw1 = CompoundTerrain Transform{toffset=(Vec3d(11,0,1)), tscale=(Vec3d(3.0,4.0,2.0)), ttheta=0.0, tphi=0.0} [SimpleTerrain boxGeometry akwTexture]
akw = CompoundTerrain nullTransform [akw1,CompoundTerrain nullTransform{toffset=Vec3d(-0.3,-0.3,0.0001)} [akw1]]

floorGeo :: Geometry
floorGeo = Quad (Vec3d(0,-1,0)) (Vec3d(1,-1,0)) (Vec3d(1,1,0)) (Vec3d(0,1,0)) Transform{toffset=(Vec3d(-250.0,0.0,0.0)), tscale=(Vec3d(1000.0,1000.0,1000.0)), ttheta=0.0, tphi=0.0}

demoFloor :: TerrainElement
demoFloor = SimpleTerrain floorGeo floorTexture

skyGeometry :: Geometry
skyGeometry = Cube 500.0 nullTransform

demoSkybox :: TerrainElement
demoSkybox = SimpleTerrain skyGeometry skyTexture

skyDomeGeo :: Geometry
skyDomeGeo = GLUQuadric (Sphere 500 1000 1000) (QuadricStyle (Just Smooth) GenerateTextureCoordinates Inside FillStyle) nullTransform

demoSkyDome :: TerrainElement
demoSkyDome = SimpleTerrain skyDomeGeo skyTexture


buildings :: TerrainElement
buildings = CompoundTerrain Transform{toffset=(Vec3d((-200.0),(-75.0),0.0)), tscale=(Vec3d(30.0,30.0,30.0)), ttheta=0.0, tphi=0.0} [tunnel, bectonBottom, akw, pillars, bectonTop]

demoTerrain, quickTerrain :: TerrainElement
demoTerrain = CompoundTerrain Transform{toffset=(Vec3d(0.0,0.0,-8.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0} [buildings, demoFloor, demoSkyDome]
quickTerrain = CompoundTerrain Transform{toffset=(Vec3d(0.0,0.0,-0.75)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0} [buildings, demoSkybox]


--------------------------------------------------------------------------------------------------------------
