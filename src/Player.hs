module Player where
import BurningPrelude
import Control.Object
import Data.Distributive (distribute)
import Data.Extensible
import qualified Block
import qualified Data.Map as Map
import Util
import Voxel
import Entity
import Data.Witherable

decFields [d|
  type Position = V3 Float
  type Position' = V3 Float
  type Velocity = V3 Float
  type AngleP = V2 Float
  type CurrentTarget = Target
  type AngleV = V2 Float
  |]

type PlayerState = AllOf [Position, Position', Velocity, AngleP, CurrentTarget, AngleV]

data Actions a where
  Jump :: Actions ()
  Move :: V2 Float -> Actions ()
  Turn :: V2 Float -> Actions ()
  Attack :: Actions ()
  Act :: Actions ()
  Update :: Float -> Actions ()
  GetPerspective :: Actions (M44 Float)

onState :: forall s t m. Monad m => s -> (forall x. t x -> StateT s m x) -> Object t m
onState s h = stateful h s

object :: Object (Public PlayerState Actions) IO
object = initial &@~ \case
  Jump -> velocity += V3 0 0.5 0
  Turn d -> do
    angleV += d
    v <- use angleV
    p <- use angleP
    angleP += (v - p) * 0.5
  Move v　-> do
    V2 dir _ <- use angleP
    position' += (V3 (angle dir) 0 (-perp (angle dir)) !* v) ^* 4
  Attack -> use currentTarget >>= \case
    TNone -> return ()
    TBlock p _ -> return ()
  Act -> use currentTarget >>= \case
    TNone -> return ()
    TBlock p s -> return ()
  Update dt -> do
    pos <- use position
    vel <- use velocity
    V2 dir elev <- use angleP
    position <~ use position'
  GetPerspective -> do
    pos <- use position
    V2 dir elev <- use angleP
    let rot = fromQuaternion $ axisAngle (V3 1 0 0) elev * axisAngle (V3 0 1 0) dir
    return $ m33_to_m44 rot !*! set translation (-pos) identity

initial = Position (V3 0 2 0)
  <% Position' (V3 0 2 0)
  <% Velocity zero
  <% AngleP zero
  <% CurrentTarget TNone
  <% AngleV zero
  <% Nil
