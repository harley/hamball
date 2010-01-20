module Vec3d where

import FRP.Yampa
import Graphics.Rendering.OpenGL

infixl 0 .*

data Vec3d = Vec3d !(GLfloat, GLfloat, GLfloat)
    deriving (Show, Eq)

scale :: Vec3d -> GLfloat -> Vec3d
scale (Vec3d (ax,ay,az)) s = Vec3d (ax*s,ay*s,az*s)

inverseScale :: Vec3d -> GLfloat -> Vec3d
inverseScale (Vec3d (ax,ay,az)) s = Vec3d (ax/s,ay/s,az/s)

(.*) :: Vec3d -> Vec3d -> GLfloat
(.*) (Vec3d (ax,ay,az)) (Vec3d (bx,by,bz)) = ax*bx + ay*by + az*bz

cross :: Vec3d -> Vec3d -> Vec3d
cross (Vec3d (a1,a2,a3)) (Vec3d (b1,b2,b3)) = Vec3d (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

len2 :: Vec3d -> GLfloat
len2 a = a .* a

len :: Vec3d -> GLfloat
len = sqrt . len2

getx,gety,getz :: Vec3d -> GLfloat
getx (Vec3d (x,y,z)) = x
gety (Vec3d (x,y,z)) = y
getz (Vec3d (x,y,z)) = z

double,float :: (Real a, Fractional b) => a -> b
double = fromRational.toRational
float = double

vector3 :: Vec3d -> Vector3 GLfloat
vector3 (Vec3d(a,b,c)) = Vector3 a b c

vertex3 :: Vec3d -> Vertex3 GLfloat
vertex3 (Vec3d(a,b,c)) = Vertex3 a b c

vec3d :: Vector3 GLfloat -> Vec3d
vec3d (Vector3 a b c) = Vec3d (a,b,c)

roundV :: Vec3d -> (Int,Int,Int)
roundV (Vec3d (x,y,z)) = (round x, round y, round z)

zipWith2 :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipWith2 f g h (x:xs) (y:ys) = (f x y) : (zipWith2 f g h xs ys)
zipWith2 f g h xs [] = map g xs
zipWith2 f g h [] ys = map h ys

instance VectorSpace Vec3d GLfloat where
    zeroVector = Vec3d (0.0, 0.0, 0.0)
    (*^) k (Vec3d (x,y,z)) = Vec3d (k*x,k*y,k*z)
    (^/) (Vec3d (x,y,z)) k = Vec3d (x/k,y/k,z/k)
    negateVector (Vec3d (x,y,z)) = Vec3d (-x,-y,-z)
    (^+^) (Vec3d (x1,y1,z1)) (Vec3d (x2,y2,z2)) = Vec3d (x1+x2,y1+y2,z1+z2)
    (^-^) (Vec3d (x1,y1,z1)) (Vec3d (x2,y2,z2)) = Vec3d (x1-x2,y1-y2,z1-z2)
    dot v1 v2 = v1 .* v2
    norm = len
    normalize v = v ^/ (norm v)

normalize' v = v ^/ (norm v)

showVec3d :: Vec3d -> String
showVec3d (Vec3d (x,y,z)) = (show x) ++ "," ++ (show y) ++ "," ++ (show z)

readVec3d :: String -> Vec3d
readVec3d s = let untilcomma = span (/= ',')
                  (x,s1) = untilcomma s
                  (y,s2) = untilcomma $ drop 1 s1
               in Vec3d (read x, read y, read $ drop 1 s2)

