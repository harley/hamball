module Player where

-- Player move according to keyboard or mouse
-- Player can shoot, etc

import Common
import FRP.Yampa
import Vec3d
import GameInput
import Graphics.Rendering.OpenGL as OpenGL
import Data.String
import WallRender
import TerrainData
--import TextureFonts
import Colors
import Render
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Sprites

renderPlayer :: Player -> IO ()
renderPlayer p = preservingMatrix $ do
    loadIdentity
    translate $ vector3 $ playerPos p
--    materialDiffuse Front $= Color4 0 0 0 1
--    materialSpecular FrontAndBack $= Color4 0 0 0 1
--    materialAmbient FrontAndBack $= Color4 0 0 0 1
    materialEmission FrontAndBack $= computeColor p
    let r = double (playerRadius p) / (sqrt 2)
    blend $= Enabled  -- this may be necessary in the future.  Right now, this functionality is encapsulated in the line above (blendEquation $= Just FuncAdd).
    blendFunc $= (One, SrcColor)  -- this defines how colors will be blended when they are drawn to the framebuffer.
                                  -- This means that transparent colors will let the background show through and opaque colors will be drawn over it.
    let
        radianToDegrees x = 180 * x / pi
        lp = round $ 100 * ((playerLife p) / maxLife)
        style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside LineStyle
    preservingMatrix $ do
        rotate (radianToDegrees $ fst $ playerView p) (vector3 $ Vec3d(0, 0, 1))
        rotate (radianToDegrees $ snd $ playerView p) (vector3 $ Vec3d(0, 1, 0))
        renderQuadric style $ Sphere (double $ playerRadius p) lp lp
        renderQuad hamsterTexture (Vertex3 (0) r (-r)) (Vertex3 (0) (r) r) (Vertex3 0 (-r) r) (Vertex3 0 (-r) (-r))

    -- mvm <- get (matrix (Modelview 0))
    -- now find the normal and rotate the image accordingly
    mv <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
    mvc <- getMatrixComponents RowMajor mv
    let offset =  float (length $ playerName p) / 2.0
        {-newMT = newMatrix RowMajor [ 1 , 0 , 0 , mvc!!3 ,
                                     0 , 1 , 0 , mvc!!7 ,
                                     0 , 0 , 1 , mvc!!11 ,
                                     0 , 0 , 0 , mvc!!15 ]-}
    preservingMatrix $ do
        loadIdentity
        --let v = Vec3d(Matrix[8], Matrix[9], -Matrix[10]);

        translate (vector3 (playerPos p ^+^ 1.1 *^ Vec3d(offset * 0.2,offset * 0.2,playerRadius p)))
        OpenGL.scale 0.05 0.05 (0.05 :: GLdouble)
        rotate (-90) (vector3 $ Vec3d(0,0,1))
        rotate 90 (vector3 $ Vec3d(1,0,0))
        -- Undo Rotations
        -- Redo Scalings
        renderString Fixed8x16 (playerName p)
        -- Use withMatrix... or access elements directly
        blend $= Disabled
        --multMatrix newMT

renderSelf :: Player -> IO()
renderSelf p = do
    let (theta,phi) = playerView p
        mkRMatrixT,mkRMatrixP,mkTMatrix :: IO (GLmatrix Float)
        (ct,st,c,s) = (cos theta,sin theta,cos phi,sin phi)
        u = Vec3d (ct,st,0) `cross` Vec3d (0,0,1)
        (x,y,z) = (getx u,gety u,getz u)
        mkRMatrixP = newMatrix RowMajor [ c , 0 , s , 0 ,
                                          0 , 1 , 0 , 0 ,
                                         -s , 0 , c , 0 ,
                                          0 , 0 , 0 , 1 ]
        mkRMatrixT = newMatrix RowMajor [ ct,-st, 0 , 0 ,
                                          st, ct, 0 , 0 ,
                                          0 , 0 , 1 , 0 ,
                                          0 , 0 , 0 , 1 ]
        mkTMatrix = newMatrix RowMajor [1,0,0,negate $ getx $ playerPos p,
                                        0,1,0,negate $ gety $ playerPos p,
                                        0,0,1,negate $ getz $ playerPos p,
                                        0,0,0,1]
     in do
        renderOrtho widthf heightf $ do
            -- the transparency blending only works if OOSSelf is rendered last, which is the case because it's first added to list -Harley
            blend $= Enabled
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)-- transparent colors will let the background show through and opaque colors will be drawn over it.
            textureFunction $= Replace
--            printFonts' (centerCoordX-9) (centerCoordY-9) (tex, base) 1 "+"

            let r = 16 :: Float
            preservingMatrix $ do
                loadIdentity
                alphaFunc $= Just (Greater,0.1:: Float)
                translate (vector3 $ Vec3d(centerCoordX, centerCoordY, 0))
                texture Texture2D $= Enabled
                displaySprite3D crosshairTexture (Vertex3 (-r) (-r) 0) (Vertex3 (-r) r 0) (Vertex3 r r 0) (Vertex3 r (-r) 0) (0.0, 0.0) (1.0, 1.0)
                texture Texture2D $= Disabled
-- TODO: color for text is not working
--            color $ Color4 0 255 0 (255 :: GLubyte)
--            renderColor (colorf Purple) do
            renderText 5 0 ("Life: " ++ show (round (playerLife p))) 4
--            printFonts' 0 0 (tex, base) 1 ("Life: " ++ show (playerLife p))
--            printFonts' 0 22 (tex, base) 1 ("Pos : " ++ show (playerPos  p))
--            printFonts' 0 44 (tex, base) 1 ("Vel : " ++ show (playerVel  p))
            blend $= Disabled

        matrixMode $= Projection
        initFrustum
        rMatrixT <- mkRMatrixT
        rMatrixP <- mkRMatrixP
        tMatrix  <- mkTMatrix
        multMatrix rMatrixP
        multMatrix rMatrixT
        multMatrix tMatrix
--        m :: GLmatrix Float <- get currentMatrix
--        mcs <- getMatrixComponents RowMajor m
--        print mcs
        matrixMode $= Modelview 0

{-
renderSelf' p =
    let xyz (theta,phi) = Vertex3 (cos theta * cos phi) (sin theta * cos phi) (sin phi)
        xyz' (theta,phi) = Vector3 (cos theta * cos phi) (sin theta * cos phi) (sin phi)
        po = playerPos p
        (th,ph) = playerView p
        theta,phi,x,y,z :: GLdouble
        theta = double th
        phi = double ph
        x = double $ getx po
        y = double $ gety po
        z = double $ getz po
     in do
        matrixMode $= Projection
        initFrustum
        lookAt (Vertex3 x y z) (xyz (theta,phi)) (Vector3 0 0 1)--(xyz' (theta, phi+pi/4))
        matrixMode $= Modelview 0
--}

instance Stringifiable Player where

    stringify p = (show $ playerID p) ++ delim ++
                  (showVec3d $ playerPos p) ++ delim ++
                  (showVec3d $ playerVel p) ++ delim ++
                  (showVec3d $ playerAcc p) ++ delim ++
                  (show $ playerView p) ++ delim ++
                  (show $ playerRadius p) ++ delim ++
                  (show $ playerLife p) ++ delim ++
                  (show $ playerEnergy p) ++ delim ++
                  (showVec3d $ playerColor p) ++ delim ++
                  (show $ playerName p)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                        (p4,s4) = untildelim $ drop 1 s3
                        (p5,s5) = untildelim $ drop 1 s4
                        (p6,s6) = untildelim $ drop 1 s5
                        (p7,s7) = untildelim $ drop 1 s6
                        (p8,s8) = untildelim $ drop 1 s7
                        (p9,s9) = untildelim $ drop 1 s8
                        (p10,s10) = untildelim $ drop 1 s9
                    in Player {playerID = read p1,
                               playerPos = readVec3d p2,
                               playerVel = readVec3d p3,
                               playerAcc = readVec3d p4,
                               playerView = read p5,
                               playerRadius = read p6,
                               playerLife = read p7,
                               playerEnergy = read p8,
                               playerColor = readVec3d p9,
                               playerName = read p10}

