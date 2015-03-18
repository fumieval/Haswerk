module Player where
import BurningPrelude
import Call
import Control.Elevator
import Control.Object
import Data.Distributive (distribute)
import Data.Extensible
import qualified Block
import qualified Data.Map as Map
import Util
import Voxel
import World
import Entity
import Data.Witherable

decFields [d|
  type Position = V3 Float
  type Position' = V3 Float
  type Velocity = V3 Float
  type AngleP = V2 Float
  type CurrentTarget = Target
  |]

type PlayerState = AllOf [Position, Position', Velocity, AngleP, CurrentTarget]

data Actions a where
  Jump :: Actions ()
  Move :: V2 Float -> Actions ()
  Turn :: V2 Float -> Actions ()
  Attack :: Actions ()
  Act :: Actions ()
  Update :: Float -> Actions ()
  GetPerspective :: Actions (Scene -> Sight)

object :: Object (Public PlayerState Actions) (StateT World IO)
object = sharing handle $ Position (V3 0 2 0)
  <% Position' (V3 0 2 0)
  <% Velocity zero
  <% AngleP zero
  <% CurrentTarget TNone
  <% Nil where
  handle :: Actions a -> StateT PlayerState (StateT World IO) a
  handle Jump = velocity += V3 0 0.5 0
  handle (Turn v) = angleP += v ^* 3
  handle (Move v) = do
    V2 dir _ <- use angleP
    position' += (V3 (angle dir) 0 (-perp (angle dir)) !* v) ^* 4
  handle Attack = use currentTarget >>= \case
    TNone -> return ()
    TBlock p _ -> lift $ apprisesOf (blocks . voxelAt p . wither) (Block.Damage 1) mempty mempty
  handle Act = use currentTarget >>= \case
    TNone -> return ()
    TBlock p s -> lift $ blocks . voxelAt (p + fromSurface s) ?= Block.stoneBrick
  handle (Update dt) = do
    pos <- use position
    vel <- use velocity
    V2 dir elev <- use angleP
    -- playerPos' += vel
    -- playerVelocity += V3 0 (-0.07) 0

    position <~ use position'
  handle GetPerspective = do
    pos <- use position
    V2 dir elev <- use angleP
    return $ viewScene (pi / 4) 1 200
      . rotateOn (V3 elev 0 0)
      . rotateOn (V3 0 dir 0)
      . translate (-pos)
