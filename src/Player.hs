module Player where

-- Player move according to keyboard or mouse
-- Player can shoot, etc

import Common
import FRP.Yampa
import Vec3d
import Graphics.Rendering.OpenGL as OpenGL
import WallRender
import TerrainData
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

    -- TODO: Fix this billboard-style name tag overhead
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

-- Camera view and text on screen.
renderSelf :: Player -> IO()
renderSelf p = do
    let (theta,phi) = playerView p
        mkRMatrixT, mkRMatrixP, mkTMatrix :: IO (GLmatrix Float)
        (ct,st,c,s) = (cos theta,sin theta,cos phi,sin phi)
        --u = Vec3d (ct,st,0) `cross` Vec3d (0,0,1)
        --(x,y,z) = (getx u,gety u,getz u)
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

            renderText 5 0 ("Life: " ++ show (round (playerLife p))) 4
            renderText 5 80 ("Pos : " ++ show (playerPos  p)) 2
            renderText 5 100 ("Vel : " ++ show (playerVel  p)) 2

            let r = 16 :: Float
            preservingMatrix $ do
                loadIdentity
                alphaFunc $= Just (Greater,0.1:: Float)
                translate (vector3 $ Vec3d(centerCoordX, centerCoordY, 0))
                texture Texture2D $= Enabled
                displaySprite3D crosshairTexture (Vertex3 (-r) (-r) 0) (Vertex3 (-r) r 0) (Vertex3 r r 0) (Vertex3 r (-r) 0) (0.0, 0.0) (1.0, 1.0)
                texture Texture2D $= Disabled
            blend $= Disabled

        matrixMode $= Projection
        initFrustum
        rMatrixT <- mkRMatrixT
        rMatrixP <- mkRMatrixP
        tMatrix  <- mkTMatrix
        multMatrix rMatrixP
        multMatrix rMatrixT
        multMatrix tMatrix
			
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
