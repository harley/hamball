module Common where
import Vec3d
import FRP.Yampa
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Data.IORef
import System.IO
import System.IO.Unsafe
import Control.Concurrent

debug :: (Show a) => a -> t -> t
debug s x = unsafePerformIO (print s) `seq` x
debugShow, debugShow2 :: (Show a) => a -> a
debugShow x = debug ("debug: " ++ show x) x -- only for SHOWable objects
debugShow2 x = debug (show x) x -- only for SHOWable objects
debugMaybe :: String -> t -> t
debugMaybe s x = if s /= "" then debug s x else x

-- The following is for debugging purposes only
--instance Show a => Show (Event a) where
--    show NoEvent = "NoEvent"
--    show (Event a) = "Event " ++ (show a)
 ----------------------------------------------

type ReactChan a = Chan (a -> a)

addToReact :: ReactChan a -> (a -> a) -> IO ()
addToReact rch f  = writeChan rch f

getReactInput :: ReactChan a -> a -> IO a
getReactInput rch old = do
    f <- readChan rch
    return $ f old

-- width MUST be divisible by 4
-- height MUST be divisible by 3

fullscreen :: Bool
fullscreen = False

width, height :: GLint
(width,height) = if fullscreen then (1600,1200) else (640,480)

widthf, heightf :: GLdouble
widthf = fromRational $ toRational width
heightf = fromRational $ toRational height

centerCoordX, centerCoordY :: Float
centerCoordX = fromIntegral width / 2
centerCoordY = fromIntegral height / 2

sensitivity :: Float
sensitivity = pi/(fromIntegral $ width `div` 4)

initFrustum :: IO ()
initFrustum = do
    loadIdentity
    let near   = 0.8
        far    = 1000
        right  = 0.4
        top    = 0.3
    frustum (-right) right (-top) top near far
	-- TODO: explain this
    lookAt (Vertex3 0 0 0) (Vertex3 1 0 0) (Vector3 0 0 1)

--bound lo hi a = max lo $ min hi a

int :: (Num b, Integral a) => a -> b
int = fromInteger.toInteger

type ID = Int
type Position3 = Vec3d
type Velocity3 = Vec3d
type Acceleration3 = Vec3d
type Color3 = Vec3d

-- data PowerUp = XRay


data Player = Player {
    playerID :: !ID,
    playerPos :: !Position3,
    playerVel :: !Velocity3,
    playerAcc :: !Acceleration3,
    playerView :: !(Float,Float), -- theta, phi
    playerRadius :: !Float,
    playerLife :: !Float,
    playerEnergy :: !Float,
    playerColor :: !Common.Color3,
    playerName :: !String
}
    deriving (Show, Eq)

data Laser = Laser {
    laserID :: !ID,
    laserpID :: !ID,
    laserPos :: !Position3,
    laserVel :: !Velocity3,
    laserStr :: !Float,
    laserColor :: !Common.Color3
}
    deriving (Show, Eq)

data Hit = Hit {
    player1ID :: !ID,
    player2ID :: !ID,
    hitLaserID :: !ID,
    hitStr :: !Float
}
    deriving (Show, Eq)

-- Particle Position Depth
data Particle = Particle {
    particlePos :: !Position3,
    particleVel :: !Vec3d,
    particleEnergy :: !Float,
    particleDepth :: !Int
}
  deriving (Show, Eq)

-- Not in use now. Instead, model as a list of ObjectSFs (representing particles). Makes simpler
data ParticleSystem = ParticleSystem {
    particlePV :: [(Vec3d,Vec3d)],
    particlesMax :: Float,
    particlesEnergy :: !Float
}
  deriving (Show, Eq)

data ScoreBoard = ScoreBoard {
    sbScores :: ![(Int, Int)] -- PlayerID and Score
}
    deriving (Show, Eq)

data PowerUpType = StrengthenLaser !Float
                 | XRayVision
                 | DecreaseRadius !Float
    deriving (Show, Eq)

data PowerUp = PowerUp {
    powerupPos :: !Position3,
    powerupRadius :: !Float,
    powerupType :: !PowerUpType
}
    deriving (Show, Eq)

-- Values for initializing objects
defRadius :: Float
defRadius = 1.5

maxLife :: Float
maxLife = 100

maxEnergy :: Float
maxEnergy = 100

defLaserStr :: Float
defLaserStr = 10

data Obj = PlayerObj !Player
         | LaserObj !Laser
    deriving (Show, Eq)

data SCMsg' = SCMsgInitialize !Player
            | SCMsgPlayer !Player        -- For updating pos
            | SCMsgHit !Hit
            | SCMsgSpawn !Obj            -- For creating new ones
            | SCMsgFrag !Hit             -- For telling everyone player1 killed player2
            | SCMsgRemove !Int             -- Remove exiting player
    deriving (Show, Eq)

data CSMsg' = CSMsgPlayer !Player        -- For when velocity changes
            | CSMsgUpdate !Player        -- For periodic updates
            | CSMsgLaser !Laser          -- For spawning
            | CSMsgKillLaser !ID
            | CSMsgDeath !Hit            -- ID of killer and killed
            | CSMsgExit !String          -- Name of player that exits, requires unique player names
            | CSMsgJoin !String          -- Name of player that enters the game
    deriving (Show, Eq)

type SCMsg = (ID, SCMsg')   -- Server to Client, i.e. runs on Client
type CSMsg = (ID, CSMsg')   -- Client to Server, i.e. runs on Server

dummySCMsg :: SCMsg
dummySCMsg = (-1,SCMsgHit (Hit {player1ID= -1,player2ID= -1,hitLaserID= -1,hitStr= -1}))

dummyCSMsg :: CSMsg
dummyCSMsg = (-1,CSMsgExit "dummy")

dummyPlayer :: Player
dummyPlayer = Player {playerID = 0,
                      playerPos = zeroVector,
                      playerVel = zeroVector,
                      playerAcc = zeroVector,
                      playerView = (0,0),
                      playerRadius = defRadius,
                      playerLife = maxLife,
                      playerEnergy = maxEnergy,
                      playerColor = Vec3d(0.5, 0.2, 0.7),
                      playerName = "Dummy"}

-- Information about the global state of the game
data GameData = GameData {
    startTime :: IORef Double,
    lastDrawTime :: IORef Double,
    numFrames :: IORef Int
}

-- Number of milliseconds between redraws
redrawTimer :: Double
redrawTimer = 0.005

-- Number of milliseconds between mouseUpdates
mouseTimer :: Double
mouseTimer = 0.001

printFlush :: String -> IO ()
printFlush s = do
    print s
    hFlush stdout
    hFlush stderr

{-
 - Removing, duplicate function 'event'
maybeEvent :: b -> (a -> b) -> Event a -> b
maybeEvent n _ NoEvent = n
maybeEvent _ f (Event x) = f x
-}

doIOevent :: Event (IO ()) -> IO ()
doIOevent (Event io) = io
doIOevent NoEvent = return ()

vecToColor :: Vec3d -> Color4 GLfloat
vecToColor (Vec3d (x,y,z)) = Color4 x y z 1

computeColor :: Player -> Color4 GLfloat
computeColor (Player {playerColor = Vec3d (r,g,b),
                      playerLife = life}) = vecToColor (Vec3d ((1 - life/maxLife) * (1-r) + r, g, b))

detectChangeSF :: Eq a => SF (a, a) (Event a, a)
detectChangeSF = arr (\(new,old) -> (if new == old then NoEvent else Event new, new))


-- selfKill e = (print "Socket closed." >> myThreadId >>= killThread >> return ())
               -- else ioError e)

-- if you change this, also go to file ./server to change the rm script to remove this file after server closes
-- hostName = ".host_name"

