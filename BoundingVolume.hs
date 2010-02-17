module BoundingVolume where

--import Common
import Vec3d

-- TODO: make collision detector return a normal vector

data BoundingVolume = BoundingBox Vec3d Vec3d   -- axis-parallel
                    | BoundingSphere Vec3d Float
                    | BoundingSegment Vec3d Vec3d
                    | BoundingEmpty             -- for objects that never collide
                    | MultipleVolumes [BoundingVolume]
    deriving (Show, Eq)

bvTransform :: (Vec3d -> Vec3d) -> BoundingVolume -> BoundingVolume
bvTransform trans (BoundingBox v1 v2) = BoundingBox (trans v1) (trans v2)
bvTransform trans (BoundingSphere p r) = BoundingSphere (trans p) r
bvTransform _ BoundingEmpty = BoundingEmpty
bvTransform trans (MultipleVolumes vols) = MultipleVolumes $ map (bvTransform trans) vols
bvTransform _ (BoundingSegment _ _) = error "Bounding segment not yet implemented."
                    
collidesWith :: BoundingVolume -> BoundingVolume -> Bool
collidesWith BoundingEmpty _ = False
collidesWith _ BoundingEmpty = False
collidesWith (BoundingBox (Vec3d (minx1,miny1,minz1)) (Vec3d (maxx1,maxy1,maxz1)))
             (BoundingBox (Vec3d (minx2,miny2,minz2)) (Vec3d (maxx2,maxy2,maxz2))) = {-debug ("CollidesWith called on " ++ (show b1) ++ " ::: " ++ (show b2) ++ " ::: returning " ++
                                                                                               (show $ minx1 < maxx2 && maxx1 > minx2 &&
                                                                                                       miny1 < maxy2 && maxy1 > miny2 &&
                                                                                                       minz1 < maxz2 && maxz1 > minz2)) $-}
                   minx1 < maxx2 && maxx1 > minx2 &&
                   miny1 < maxy2 && maxy1 > miny2 &&
                   minz1 < maxz2 && maxz1 > minz2
collidesWith (MultipleVolumes vols) bv = or $ map (collidesWith bv) vols
collidesWith bv (MultipleVolumes vols) = or $ map (collidesWith bv) vols
collidesWith _ _ = error "collidesWith not yet implemented for this bounding vaolume pattern"
  
