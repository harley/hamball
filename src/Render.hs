{-# LANGUAGE RankNTypes #-}
{-****************************************************************************
*                              Hamster Balls                                 *
*       Purpose:   Useful & common rendering routines used on the client     *
*       Author:    David, Harley, Alex, Matt                                 *
*             Copyright (c) Yale University, 2010                            *
****************************************************************************-}
module Render where

import Graphics.Rendering.OpenGL as OpenGL
import Vec3d
import Common
import Graphics.UI.GLFW
import Sprites

-- Technically this renders any text
renderKillText :: String -> IO()
renderKillText str = do
  renderOrtho widthf heightf $ do
            -- the transparency blending only works if OOSSelf is rendered last, which is the case because it's first added to list -Harley
            blend $= Enabled
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)-- transparent colors will let the background show through and opaque colors will be drawn over it.
            textureFunction $= Replace
            renderText 5 200 str 3

renderScoreBoard :: ScoreBoard -> IO ()
renderScoreBoard sb =
    let mergeSort [] = []
        mergeSort [x] = [x]
        mergeSort l = let (l1,l2) = foldl (\(l1,l2) a -> (l2,a:l1)) ([],[]) l
                      in merge (mergeSort l1) (mergeSort l2)
            where merge [] l = l
                  merge l [] = l
                  merge l1@(x:xs) l2@(y:ys) = if snd x > snd y then x:(merge xs l2) else y:(merge l1 ys)
    in renderOrtho widthf heightf $ do
           blend $= Enabled
           blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
           textureFunction $= Replace
           let loop n ((plID,s):rest) = do
                   renderText 10 (double heightf - n*64) (show plID ++ ": " ++ show s) 4
                   loop (n+1) rest
               loop _ [] = return ()
           loop 1 $ mergeSort $ sbScores sb

-- sets up the orthographic mode so we can
-- draw at 2D screen coordinates
renderOrtho :: GLdouble -> GLdouble -> IO a -> IO()
renderOrtho width height graphicActions = do
   matrixMode   $= Projection
   unsafePreservingMatrix $ do
     loadIdentity
     ortho 0 width 0 height (-1) 1
     matrixMode $= Modelview 0
     graphicActions
     matrixMode $= Projection
   matrixMode   $= Modelview 0

--render text on 2D on front of screen
renderText :: Float -> Float -> String -> Float -> IO ()
renderText x y str s = unsafePreservingMatrix $ do
    translate (Vector3 x y (0::Float))
    OpenGL.scale s s (1::Float)
    renderString Fixed8x16 str

renderColor :: forall a. Color4 GLfloat -> IO a -> IO a
renderColor c graphicActions = unsafePreservingMatrix $ do
    -- clear [ColorBuffer]
	{-
    let curDiff = materialDiffuse FrontAndBack
        curSpec = materialSpecular FrontAndBack
        curAmb = materialAmbient FrontAndBack
        curEmis = materialEmission FrontAndBack
		-}
    -- Store colors
    materialDiffuse FrontAndBack $= c -- For now, always FrontAndBack
    materialSpecular FrontAndBack $= c
    materialAmbient FrontAndBack $= c
    materialEmission FrontAndBack $= c
    -- materialShininess FrontAndBack $= shin
    graphicActions -- Execute commands

{-  -- restore?
    materialDiffuse FrontAndBack $= curDiff -- For now, always FrontAndBack
    materialSpecular FrontAndBack $= curSpec
    materialAmbient FrontAndBack $= curAmb
    materialEmission FrontAndBack $= curEmis
-}

renderQuad ::  Maybe TextureObject -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
renderQuad mbTexObj p1 p2 p3 p4 = do
    texture Texture2D $= Enabled
    textureFunction $= Decal
    textureBinding Texture2D $= mbTexObj

    displaySprite3D mbTexObj p1 p2 p3 p4 (0,0) (1,1)
    texture Texture2D $= Disabled

