module WallRender where

import Graphics.Rendering.OpenGL
import Sprites

renderQuad ::  Maybe TextureObject -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
renderQuad mbTexObj p1 p2 p3 p4 = do
    texture Texture2D $= Enabled
    textureFunction $= Decal
    textureBinding Texture2D $= mbTexObj

    displaySprite3D mbTexObj p1 p2 p3 p4 (0,0) (1,1)
    texture Texture2D $= Disabled

displayCB :: Maybe TextureObject -> IO ()
displayCB mbTexObj = do
    clear [ ColorBuffer, DepthBuffer ]
    renderQuad mbTexObj (Vertex3 0 0 0) (Vertex3 0 2 0) (Vertex3 2 2 0) (Vertex3 2 0 0)
    flush

