{-
This module provides the necessary tools for drawing flat sprites using OpenGL.
Sprites are simply drawn as quads (sets of four points) with textures applied.  This module provides functions for easily setting up the environment, loading textures, and drawing them to the screen.
-}

module Sprites (spriteInit, spriteDisable, createTexture, createTextures, displaySprite, displaySpriteWithFrame, displaySprite3D) where

import ReadImage (readImage)
import Monad (when)
import Graphics.Rendering.OpenGL

{-
This function sets the OpenGL environment up to handle translucent sprites.
This function makes use of some parts of HOpenGL which are changing, and so attempting to compile it with the latest libraries may not work.  We have marked up the parts which may become obsolete in the near future.
This function should be called after OpenGL has set up its window, but before drawing occurs.
-}

spriteInit :: Int -> Int -> IO ()  -- this function takes two Int's, the screen length and the screen height.  Note that these numbers do not need to corrospond to the window size in any way.
spriteInit x y = do
	blendEquation $= FuncAdd  --
	blend $= Enabled  -- this may be necessary in the future.  Right now, this functionality is encapsulated in the line above (blendEquation $= Just FuncAdd).
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)  -- this defines how colors will be blended when they are drawn to the framebuffer.  This means that transparent colors will let the background show through and opaque colors will be drawn over it.
	textureFunction $= Replace  -- when textures are applied to our polygons, we would like for their color and alpha information to replace.
	texture Texture2D $= Enabled  -- enable 2D textures.
	clearColor $= Color4 0.0 0.0 0.0 0.0  -- we want the framebuffer's clear color to be black (we're really just doing this for the alpha information).
	color (Color4 0.0 0.0 0.0 (1.0 :: GLfloat))  -- we define the color that all of the quads will be before textures are applied.  We want it to have an alpha value of 1.0.
	ortho 0.0 (fromIntegral x) 0.0 (fromIntegral y) (-1.0) (1.0)  -- this is somewhat of a hack.  Think of it as defining how many pixels large the screen is.

spriteDisable :: IO ()
spriteDisable = do
    blend $= Disabled
    texture Texture2D $= Disabled
    return ()
{-
This function provides a way to load a raw image file into memory.  If the load fails, this function will return Nothing.
The readImage function called in this routine is part of a module called ReadImage which was written by Sven Panne.  The function has been slightly modified to read alpha data.
-}

createTexture :: FilePath -> (Bool, Bool) -> IO (Maybe TextureObject)  -- the user must pass a FilePath to the desired image and a Bool tuple which determines whether the texture is repeated in the x and y directions.
createTexture filename (repeatX, repeatY) = preservingMatrix $ do
	[texName] <- genObjectNames 1  -- generate our texture.
	textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
	when repeatX (textureWrapMode Texture2D S $= (Repeated, Repeat))  -- define wrapping along the x axis.
	when repeatY (textureWrapMode Texture2D T $= (Repeated, Repeat))  -- define wrapping along the y axis.
	textureFilter Texture2D $= ((Nearest, Nothing), Nearest)  -- ?  This is necessary, but I don't know what it does.
	((Size x y), pixels) <- readImage filename  -- read our image into a PixelData structure.
	texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D x y) 0 pixels  -- associate our image with our new texture.  Since we are dealing with sprites, we do not wish to create mipmaps.
	return (Just texName)  -- return our (Maybe TextureObject) for later use.


{-
This function is provided as a convenience, and can be used to load multiple textures at once.
-}

createTextures :: [(FilePath, (Bool, Bool))] -> IO [(Maybe TextureObject)]
createTextures parameters = mapM (uncurry createTexture) parameters


{-
This function will draw a sprite to the screen, but by displaying only a certain part of the sprite's texture.
This function is meant for animation whose frames are all stored in a single texture.
This function uses the user-provided indexing-function to translate an animation frame number into a rectangle on the texture.
-}

displaySpriteWithFrame :: Maybe TextureObject -> (Int, Int) -> (Int, Int) -> (Int -> ((GLfloat, GLfloat), (GLfloat, GLfloat))) -> Int -> GLfloat -> IO ()
displaySpriteWithFrame image minn maxx func frame angle = displaySpriteBackend image (findCenterBackend minn maxx) (findSizeBackend minn maxx) texMin texMax angle
	where (texMin, texMax) = func frame


{-
This function will draw a sprite to the screen.  It is provided as a useful frontend.
The user must provide the lower-left hand Integer coordinates of the sprite and the upper-right hand coordinates as well as the lower-left and upper-right texture coordinates to draw from.  A rotation angle can also be specified.
-}

displaySprite :: Maybe TextureObject -> (Int, Int) -> (Int, Int) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat -> IO ()
displaySprite image min max texMin texMax angle = displaySpriteBackend image (findCenterBackend min max) (findSizeBackend min max) texMin texMax angle


{-
This function is called by both displaySpriteWithFrame and displaySprite, and it does all of the hard work.

In order, this function takes a (Maybe TextureObject), which is our texture,
a GLfloat pair which is screen coordinates of the sprite's center,
a GLfloat pair which is half of the sprite's size in each dimension,
two GLfloat pairs which are the lower-left and upper-right texture coordinates,
and a GLfloat which is the sprite's angle of rotation.
-}

displaySpriteBackend :: Maybe TextureObject -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat -> IO ()
displaySpriteBackend image (cx, cy) (sx, sy) (tx0, ty0) (tx1, ty1) angle = do
	textureBinding Texture2D $= image  -- set our (Maybe TextureObject) as our current texture.
	preservingMatrix $ do  -- since our screen coordinates and rotation angle are specific to this sprite, we wish for the drawing operations to take place while preserving our original matrix.
		translate $ Vector3 cx cy 0  -- set the sprite's translated coordinates.
		rotate angle $ Vector3 0 0 1  -- set the sprite's rotation.
		let verts = [(Vertex3 (-sx) (-sy) 0), (Vertex3 (-sx) (sy) 0), (Vertex3 (sx) (sy) 0), (Vertex3 (sx) (-sy) 0)]  -- define the verticies of our quad.
		    texs = [(TexCoord2 tx0 ty1), (TexCoord2 tx0 ty0), (TexCoord2 tx1 ty0), (TexCoord2 tx1 ty1)]  -- define the corrosponding texture coordinates.
		renderPrimitive Quads $ do mapVerticies texs verts  -- draw the entire quad, textures and all.

displaySprite3D :: Maybe TextureObject
                        -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
                        -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
displaySprite3D image p1 p2 p3 p4 (tx0, ty0) (tx1, ty1) = do
	textureBinding Texture2D $= image  -- set our (Maybe TextureObject) as our current texture.
	preservingMatrix $ do
		let verts = [p1, p2, p3, p4]    -- verticies of our quad.
		    texs = [(TexCoord2 tx0 ty1), (TexCoord2 tx0 ty0), (TexCoord2 tx1 ty0), (TexCoord2 tx1 ty1)]  -- define the corrosponding texture coordinates.
		renderPrimitive Quads $ do mapVerticies texs verts  -- draw the entire quad, textures and all.


{-
This function takes the lower-left hand and upper-right hand coordinates of a sprite rectangle and determines where the center of the rectangle is.
The result of this function is passed into displaySpriteBackend.
-}

findCenterBackend :: (Int, Int) -> (Int, Int) -> (GLfloat, GLfloat)
findCenterBackend (x0, y0) (x1, y1) = (((fromIntegral (x1 - x0)) / 2) + (fromIntegral x0), ((fromIntegral (y1 - y0)) / 2) + (fromIntegral y0))


{-
This function takes the lower-left hand and upper-right hand coordinates of a sprite rectangle and determines the rectangle's size.
The result of this function is passed into displaySpriteBackend.
-}

findSizeBackend :: (Int, Int) -> (Int, Int) -> (GLfloat, GLfloat)
findSizeBackend (x0, y0) (x1, y1) = ((fromIntegral (x1 - x0)) / 2, (fromIntegral (y1 - y0)) / 2)


{-
A routine used to set coordinates.  Called by mapVerticies.
-}

setVertex :: (TexCoord2 GLfloat, Vertex3 GLfloat) -> IO ()
setVertex (texCoordinates, vertexCoordinates) = do texCoord texCoordinates; vertex vertexCoordinates;


{-
A routine used to draw a list of coordinates.  Called by displaySpriteBackend.
-}

mapVerticies :: [(TexCoord2 GLfloat)] -> [(Vertex3 GLfloat)] -> IO ()
mapVerticies texs verts = mapM_ setVertex (zip texs verts)
