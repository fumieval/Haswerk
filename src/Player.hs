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
  GetPerspective :: Actions (Scene -> Sight)

onState :: forall s t m. Monad m => s -> (forall x. t x -> StateT s m x) -> Object t m
onState s h = stateful h s

object :: Object (Public PlayerState Actions) (StateT World IO)
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
    TBlock p _ -> lift $ do
      () <- apprisesOf (blocks . at p . wither) (Block.Damage 1) mempty mempty
      causeBlockUpdate p
  Act -> use currentTarget >>= \case
    TNone -> return ()
    TBlock p s -> lift $ placeBlock (p + fromSurface s) Block.stoneBrick
  Update dt -> do
    pos <- use position
    vel <- use velocity
    V2 dir elev <- use angleP
    position <~ use position'
  GetPerspective -> do
    pos <- use position
    V2 dir elev <- use angleP
    let rot = fromQuaternion $ axisAngle (V3 1 0 0) elev * axisAngle (V3 0 1 0) dir
    let !m = m33_to_m44 rot !*! set translation (-pos) identity
    return $ viewScene (pi / 4) 1 360
      . \(Scene r) -> Scene $ applyMatrix m r

initial = Position (V3 0 2 0)
  <% Position' (V3 0 2 0)
  <% Velocity zero
  <% AngleP zero
  <% CurrentTarget TNone
  <% AngleV zero
  <% Nil
