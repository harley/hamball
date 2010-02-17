module Terrain where

import Vec3d
import Graphics.Rendering.OpenGL hiding (Texture)
import GHC.Float
import WallRender
import BoundingVolume
import Data.Maybe
{---------------------------------------------------------------------------------------------------------------
TO DO:
Add orientation and scale to definition of TerrainElement

Existential types for the bounding boxes

Apply transformations to bounding boxes

Restore colors (do not need this, for now)

Only allow compound terrains to have associated transforms
  This ways is much cleaner, but either way should be fine.

MAYBE:
Generate commands instead of executing them (easier for textures). Then, in
renderTerrainElement, just generate commands and execute them.
---------------------------------------------------------------------------------------------------------------}
data Col = Col{cspecular :: Color4 GLfloat,
               cdiffuse :: Color4 GLfloat,
               cambient :: Color4 GLfloat,
               cemissive :: Color4 GLfloat,
               cshininess :: GLfloat}
    deriving Show

data Transform = Transform{toffset :: Vec3d,
                           tscale :: Vec3d, -- Also let orientation be set
                           ttheta :: Float,
                           tphi :: Float}
    deriving Show


data Surface = Color Col
             | Texture TextureObject -- Has how to render as well
             | Mirror -- Mirror and Transparent are essentially the same (get the framebuffer?)
             | Transparent
             | NoSurface
    deriving Show

{-
-- Hmm lazy way: orphan instance
instance Show QuadricPrimitive where
  show _ = "QuadricPrimitive"

instance Show QuadricStyle where
  show _ = "QuadricStyle"
-}

-- Maybe not best form (only CompoundTerrain should have transform). But saves lots of work
data Geometry = GLUQuadric QuadricPrimitive QuadricStyle Transform
                  | Cube Height Transform
                  | Quad Vec3d Vec3d Vec3d Vec3d Transform
                  | Tri Vec3d Vec3d Vec3d Transform
                  | Plane -- For later
                  | Trimesh -- For later
--    deriving Show

data TerrainElement = SimpleTerrain Geometry Surface -- Is Flavour part of TerrainElement or of Geometry?
                    | CompoundTerrain Transform [TerrainElement]
--    deriving Show
instance Show TerrainElement where
    show _ = "TerrainElement"

-- Restore surface color properties
restoreSurfaceColor :: IO()
restoreSurfaceColor = do
  materialDiffuse FrontAndBack $= Color4 0.0 0.0 0.0 1.0 -- For now, always FrontAndBack
  materialSpecular FrontAndBack $= Color4 0.0 0.0 0.0 1.0
  materialAmbient FrontAndBack $= Color4 0.0 0.0 0.0 1.0
  materialEmission FrontAndBack $= Color4 0.0 0.0 0.0 1.0




-- Execute commands while preserving surface characteristics
-- Still have issues for texturing

preservingSurface :: Surface -> IO() -> IO()
preservingSurface (Terrain.Color (Col{cspecular=spec, cdiffuse=diff, cambient=amb,cemissive=emis,cshininess=shin})) commands = do
  -- clear [ColorBuffer]
  {-
  let curDiff = materialDiffuse FrontAndBack
      curSpec = materialSpecular FrontAndBack
      curAmb = materialAmbient FrontAndBack
      curEmis = materialEmission FrontAndBack
  -}  
    -- Store colors
  materialDiffuse FrontAndBack $= diff -- For now, always FrontAndBack
  materialSpecular FrontAndBack $= spec
  materialAmbient FrontAndBack $= amb
  materialEmission FrontAndBack $= emis
  -- materialShininess FrontAndBack $= shin
  commands -- Execute commands
  restoreSurfaceColor
  -- flush
  -- clearColor $= Color4 0.0 0.0 0.0 0.0 -- Reset colors (not working right now)
preservingSurface (Terrain.Texture tex) commands = do
--  materialDiffuse FrontAndBack $= Color4 0.4 0.5 0.6 1
  texture Texture2D $= Enabled
  textureFunction $= Decal
  textureBinding Texture2D $= Just tex
  commands
  texture Texture2D $= Disabled
--  return ()

preservingSurface NoSurface commands = do
  commands

preservingSurface _ commands = do
  print "Surface functionality not yet implemented"
  commands
{-
preservingSurface (Terrain.Texture tex) commands = do
--  materialDiffuse FrontAndBack $= Color4 0.4 0.5 0.6 1
  texture Texture2D $= Enabled
  textureFunction $= Decal
  textureBinding Texture2D $= Just tex
  commands
  texture Texture2D $= Disabled
--  return ()

-- Then
renderTerrainElement (SimpleTerrain (Quad p1 p2 p3 p4 transform) (Texture texObj) flav) = do
--  print "Rendering quad with texture"
  loadIdentity
  preservingMatrix $ do
    preservingSurface (Texture texObj) $ do
      --loadIdentity
      applyTransform transform
      displaySprite3D (Just texObj) (vertex3 p1) (vertex3 p2) (vertex3 p3) (vertex3 p4) (0,0) (1,1)
      -- renderQuad (Just texObj) (vertex3 p1) (vertex3 p2) (vertex3 p3) (vertex3 p4)

-}

applyTransform :: Transform -> IO()
applyTransform Transform{toffset=offset, tscale=Vec3d(sx,sy,sz), ttheta = thetaAngle, tphi = phiAngle} = do
    translate $ vector3 offset
    Graphics.Rendering.OpenGL.scale sx sy sz
    rotate thetaAngle $ vector3 (Vec3d(0.0, 0.0, 1.0))
    rotate phiAngle $ vector3 (Vec3d(1.0, 0.0, 0.0))


applyTransform2 :: Transform -> Vec3d -> Vec3d
applyTransform2 Transform{toffset=Vec3d(dx,dy,dz), tscale=Vec3d(sx,sy,sz), ttheta = thetaAngle, tphi = phiAngle} (Vec3d (x,y,z)) =
    Vec3d (sx*x + dx, sy*y + dy, sz*z + dz) -- Bounding boxes are axis aligned... but still may need to make slightly larger

renderTerrainElement :: TerrainElement -> IO()
renderTerrainElement t@(SimpleTerrain (GLUQuadric qprimitive qstyle transform) surf) = do
--  print "Rendering GLUQuadric"
  preservingMatrix $ do
    preservingSurface surf $ do
      applyTransform transform
      renderQuadric qstyle qprimitive
--      renderBoundingVolume $ getTerrainBounds t

renderTerrainElement (SimpleTerrain (Cube height transform) (Texture texObj)) = do
--  print "Rendering glutobject with texture"
--  loadIdentity
  preservingMatrix $ do
    preservingSurface (Texture texObj) $ do
--      loadIdentity
      let h2 = (float height)/2
          p1 = Vertex3 (h2) (h2) (h2)
          p2 = Vertex3 (h2) (h2) (-h2)
          p3 = Vertex3 (h2) (-h2) (-h2)
          p4 = Vertex3 (h2) (-h2) (h2)
          p5 = Vertex3 (-h2) (h2) (h2)
          p6 = Vertex3 (-h2) (h2) (-h2)
          p7 = Vertex3 (-h2) (-h2) (-h2)
          p8 = Vertex3 (-h2) (-h2) (h2)

      applyTransform transform
      renderQuad (Just texObj) p1 p2 p3 p4
      renderQuad (Just texObj) p1 p5 p8 p4
      renderQuad (Just texObj) p5 p6 p7 p8
      renderQuad (Just texObj) p6 p7 p3 p2
      renderQuad (Just texObj) p1 p2 p6 p5
      renderQuad (Just texObj) p3 p4 p8 p7

--handle texture
renderTerrainElement (SimpleTerrain (Quad p1 p2 p3 p4 transform) (Texture texObj)) = do
--  print "Rendering quad with texture"
--  loadIdentity
  preservingMatrix $ do
    preservingSurface (Texture texObj) $ do
      --loadIdentity
      applyTransform transform
      renderQuad (Just texObj) (vertex3 p1) (vertex3 p2) (vertex3 p3) (vertex3 p4)

renderTerrainElement (SimpleTerrain (Quad p1 p2 p3 p4 transform) surf) = do
--  print "Rendering quad"
--  loadIdentity
  preservingMatrix $ do
    preservingSurface surf $ do
--      loadIdentity
      applyTransform transform
      renderPrimitive Quads $ do
        clear [ColorBuffer]
        -- color $ (Color3 (1.0::GLfloat) 0 0)
        -- materialDiffuse FrontAndBack $= Color4 1.0 0.0 0.0 1.0
        -- materialEmission FrontAndBack $= Color4 1.0 0.0 0.0 1.0
        vertex $ vertex3 p1
        vertex $ vertex3 p2
        vertex $ vertex3 p3
        vertex $ vertex3 p4

renderTerrainElement (SimpleTerrain (Tri p1 p2 p3 transform) surf) = do
--  print "Rendering quad"
--  loadIdentity
  preservingMatrix $ do
    preservingSurface surf $ do
--      loadIdentity
      blend $= Enabled
      blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
      applyTransform transform
      renderPrimitive Triangles $ do
        clear [ColorBuffer]
        -- color $ (Color3 (1.0::GLfloat) 0 0)
        -- materialDiffuse FrontAndBack $= Color4 1.0 0.0 0.0 1.0
        -- materialEmission FrontAndBack $= Color4 1.0 0.0 0.0 1.0
        vertex $ vertex3 p1
        vertex $ vertex3 p2
        vertex $ vertex3 p3


renderTerrainElement (SimpleTerrain (Trimesh) _) = do
  print "Rendering trimesh terrain"
renderTerrainElement (CompoundTerrain transform telements) = do
  -- print "Rendering compound terrain"
--  loadIdentity
  preservingMatrix $ do
    applyTransform transform
    foldr (>>) (return ()) $ map renderTerrainElement telements
renderTerrainElement _ = error "Undefined terrain element"

-- Need to consider transforms
getTerrainBounds :: TerrainElement -> BoundingVolume

getTerrainBounds (SimpleTerrain (Cube height trans) _) =
    BoundingBox (applyTransform2 trans (Vec3d (-h2,-h2,-h2))) (applyTransform2 trans (Vec3d (h2,h2,h2)))
        where h2 = float height/2

getTerrainBounds (SimpleTerrain (GLUQuadric (Cylinder innerRad outerRad height _ _) _ trans) _) =
    BoundingBox (applyTransform2 trans (Vec3d (-maxR,-maxR,0.0))) (applyTransform2 trans (Vec3d (maxR,maxR,h)))
        where h = float height
              maxR = max (float outerRad) (float innerRad)

{-
getTerrainBounds (SimpleTerrain (GLUTObject (Sphere' radius _ _) Transform{toffset=position}) _ _) =
        BoundingEmpty
getTerrainBounds (SimpleTerrain (GLUTObject _ Transform{toffset=position}) _ _) = -- Other GLUT objects. Implement later
        BoundingEmpty
-}
getTerrainBounds (CompoundTerrain trans telements) =
        MultipleVolumes $ map (bvTransform (applyTransform2 trans) . getTerrainBounds) telements
getTerrainBounds (SimpleTerrain (Quad p1 p2 p3 p4 trans) _) =
        BoundingBox (Vec3d (minx,miny,minz)) (Vec3d (maxx,maxy,maxz))
            where ps = map (applyTransform2 trans) [p1,p2,p3,p4]
                  minx = minimum $ map getx ps
                  miny = minimum $ map gety ps
                  minz = minimum $ map getz ps
                  maxx = maximum $ map getx ps
                  maxy = maximum $ map gety ps
                  maxz = maximum $ map getz ps
getTerrainBounds _ = BoundingEmpty

{-
processSurface :: Surface -> IO()
processSurface (Terrain.Color (Col{cspecular=spec, cdiffuse=diff, cambient=amb,cemissive=emis,cshininess=shin})) =  do
  clearColor $= Color4 0.0 0.0 0.0 0.0
  materialDiffuse FrontAndBack $= diff -- For now, always FrontAndBack
processSurface (Terrain.Texture) = do
  print "Processing texture"
processSurface _ =  do
  print "Surface functionality not yet implemented"
  materialDiffuse FrontAndBack $= Color4 1.0 1.0 1.0 1.0 -- Default to white
-}


{-
preservingTransform :: Transform -> IO() -> IO()
preservingTransform Transform{toffset=offset, tscale=Vec3d(sx,sy,sz), ttheta = thetaAngle, tphi = phiAngle} commands = do
  translate $ vector3 offset
  Graphics.Rendering.OpenGL.scale sx sy sz
  preservingMatrix $ do
    Graphics.Rendering.OpenGL.scale sx sy sz
    translate $ vector3 offset
    rotate thetaAngle $ vector3 (Vec3d(0.0, 0.0, 1.0))
    rotate phiAngle $ vector3 (Vec3d(1.0, 0.0, 0.0))
    commands
-}

-- For debugging purposes
{-
tempTex = (Texture (fromJust $ unsafePerformIO (getAndCreateTexture "bricks")))

renderBoundingVolume (BoundingBox v1 v2) = do
  renderTerrainElement (SimpleTerrain (Cube 1.0 transform) tempTex)
  print "Coordinates are:"
  print v1
  print v2
  where height = (getz v2) ^-^ (getz v1)
        transform = Transform{toffset=Vec3d(0.0,0.0,height/2),tscale=(v2 ^-^ v1), ttheta=0.0,tphi=0.0}
-}

