{-# LANGUAGE LambdaCase, TemplateHaskell, GADTs, TypeOperators #-}
module Player where
import Call.Sight
import Linear
import Control.Lens
import World
import Voxel
import Control.Monad.State.Strict
import Control.Object
import Data.Distributive (distribute)
import qualified Data.Map as Map
import Util
import Data.List (minimumBy)
import Data.Function (on)
import Data.OpenUnion1.Clean

data PlayerState = PlayerState
  { _position :: V3 Float
  , _position' :: V3 Float
  , _velocity :: V3 Float
  , _angleP :: V2 Float
  , _focused :: Maybe (V3 Int, V3 Int)
  }
makeLenses ''PlayerState

data Actions a where
  Jump :: Actions ()
  Move :: V2 Float -> Actions ()
  Turn :: V2 Float -> Actions ()
  Attack :: Actions ()
  Act :: Actions ()
  Update :: Float -> Actions ()
  GetPerspective :: Actions (Scene -> Sight)

object :: Monad m => Object (State PlayerState |> Actions |> Nil) (StateT World m)
object = sharing handle $ PlayerState (V3 0 2 0) (V3 0 2 0) zero zero Nothing where
  handle :: Monad m => Actions a -> StateT PlayerState (StateT World m) a
  handle Jump = velocity += V3 0 0.5 0
  handle (Turn v) = angleP += v ^* 4
  handle (Move v) = do
    V2 dir _ <- use angleP
    position' += (V3 (angle dir) 0 (-perp (angle dir)) !* v) ^* 6
  handle Attack = use focused >>= \case
    Just (p, _) -> lift $ blocks . voxelAt p .= Nothing
    Nothing -> return ()
  handle Act = use focused >>= \case
    Just (p, n) -> lift $ blocks . voxelAt (p + n) ?= Block StoneBrick
    Nothing -> return ()
  handle (Update dt) = do
    pos <- use position
    vel <- use velocity
    V2 dir elev <- use angleP
    -- playerPos' += vel
    -- playerVelocity += V3 0 (-0.07) 0


    position <~ use position'

    w <- lift get
    focused .= getFocus w pos (spherical dir elev)
  handle GetPerspective = do
    pos <- use position
    V2 dir elev <- use angleP
    return $ viewScene (pi / 4) 1 200
      . rotateOn (V3 elev 0 0)
      . rotateOn (V3 0 dir 0)
      . translate (-pos)

getFocus :: World -> V3 Float -> V3 Float -> Maybe (V3 Int, V3 Int)
getFocus w pos aim = case [v | (i, s) <- ss ^@.. itraversed <. folded
  , fmap fromIntegral i `qd` pos < 8*8
  , v@(_, (p, n)) <- facing i pos aim (fromSurface s)
  , Map.notMember (p + n) bs] of
  [] -> Nothing
  xs -> Just $ snd $ minimumBy (compare `on` fst) xs
  where
    VoxelWorld bs ss = _blocks w