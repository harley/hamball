{- Most of the code here (except the collision detection) is adapted from Section 5.5 of the Yampa Arcade paper -}

module GameCore where

import IdentityList
import FRP.Yampa
import Object
import GameInput
import Common
import BoundingVolume
import Data.List

{- dpSwitch :: Functor col =>       (col is a collection)
     (forall sf . (a -> col sf -> col (b, sf))) ->
     col (SF b c) ->
     SF (a, col c) (Event d) ->
     (col (SF b c) -> d -> SF a (col c)) ->
     SF a (col c)
-}

gameCore :: IL ObjectSF -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
gameCore initObjs = dpSwitch route initObjs (arr killOrSpawn >>> notYet) (\sfs f -> gameCore (f sfs))

route :: (GameInput, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
route (gi,oos) objs = mapIL (route' oos) objs
    where route' oos (k,obj) = (ObjInput {oiGameInput = gi, oiColliding = case getIL k oos of
                                                                              Just oo -> find (collidesWith (ooBounds oo) . ooBounds) $ map snd $ assocsIL $ deleteIL k oos
                                                                              Nothing -> Nothing}, obj)

killOrSpawn :: (a, IL ObjOutput) -> Event (IL ObjectSF -> IL ObjectSF)
killOrSpawn (_,oos) = Event $ foldl (.) id funcs
    where funcs = map (\(k,oo) -> ((maybeEvent id (\_ -> deleteIL k)) (ooKillReq oo)) . ((foldl (.) id . map insertIL) (ooSpawnReq oo))) (assocsIL oos)
