module WallRender where

import Graphics.Rendering.OpenGL
import Control.Monad ( when )
import Data.Maybe ( isJust, listToMaybe )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Sprites

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

renderQuad ::  Maybe TextureObject -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
renderQuad mbTexObj p1 p2 p3 p4 = do
    texture Texture2D $= Enabled
    textureFunction $= Decal
    textureBinding Texture2D $= mbTexObj

    displaySprite3D mbTexObj p1 p2 p3 p4 (0,0) (1,1)
    texture Texture2D $= Disabled

displayCB mbTexObj = do
    clear [ ColorBuffer, DepthBuffer ]
    renderQuad mbTexObj (Vertex3 0 0 0) (Vertex3 0 2 0) (Vertex3 2 2 0) (Vertex3 2 0 0)
    flush

-- load only this file and run 'test' to test
{-test :: IO ()
test = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 200 200
    createWindow progName
    mbTexObj <- wallInit
    displayCallback $= displayCB mbTexObj
    mainLoop
    -}
