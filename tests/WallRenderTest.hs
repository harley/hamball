-- defunct
module WallRenderTest where

import Graphics.Rendering.OpenGL
import Sprites
import WallRender
import SpritesTest

--Texture objects are only used when GL_EXT_texture_object is supported.
wallInit :: IO (Maybe TextureObject)
wallInit = do
{-   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   exts <- get glExtensions
   mbTexName <- if "GL_EXT_texture_object" `elem` exts
                   then fmap listToMaybe $ genObjectNames 1
                   else return Nothing
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName

   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withCheckImage checkImageSize 0x8 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
   return mbTexName
   -}
   tx <- createTexture "test6.rgb" (False, False)
   return tx

test :: IO ()
test = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 200 200
    createWindow progName
    mbTexObj <- wallInit
    displayCallback $= displayCB mbTexObj
    mainLoop

