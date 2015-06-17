module Player where
import BurningPrelude
import Data.Extensible
import Entity

mkField "position position' velocity angleP currentTarget angleV"

type PlayerState = Record '["position" :> V3 Float
  , "position'" :> V3 Float
  , "velocity" :> V3 Float
  , "angleP" :> V2 Float
  , "currentTarget" :> Target
  , "angleV" :> V2 Float]

jump :: StateT PlayerState IO ()
jump = velocity += V3 0 0.5 0

turn :: V2 Float -> StateT PlayerState IO ()
turn d = do
  angleV += d
  v <- use angleV
  p <- use angleP
  angleP += (v - p) * 0.5

move :: V2 Float -> StateT PlayerState IO ()
move v　= do
  V2 dir _ <- use angleP
  position' += (V3 (angle dir) 0 (-perp (angle dir)) !* v) ^* 4

attack :: StateT PlayerState IO ()
attack = use currentTarget >>= \case
  TNone -> return ()
  TBlock p _ -> return ()

act :: StateT PlayerState IO ()
act = use currentTarget >>= \case
  TNone -> return ()
  TBlock p s -> return ()

update :: Float -> StateT PlayerState IO ()
update dt = do
  pos <- use position
  vel <- use velocity
  V2 dir elev <- use angleP
  position <~ use position'

getPerspective :: StateT PlayerState IO (M44 Float)
getPerspective = do
  pos <- use position
  V2 dir elev <- use angleP
  let rot = fromQuaternion $ axisAngle (V3 1 0 0) elev * axisAngle (V3 0 1 0) dir
  return $ m33_to_m44 rot !*! set translation (-pos) identity

initial :: PlayerState
initial = position @= V3 0 2 0
  <: position' @= V3 0 2 0
  <: velocity @= zero
  <: angleP @= zero
  <: currentTarget @= TNone
  <: angleV @= zero
  <: Nil
