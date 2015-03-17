module Util where
import Data.Foldable as F
import Linear
import Control.Object
import Control.Elevator
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Control.Monad

play :: Metric f => Float -> f Float -> f Float
play t v
  | quadrance v < t*t = zero
  | otherwise = v

accel :: V2 Float -> V2 Float
accel v = v ^* quadrance v ** 0.3

spherical :: RealFloat a => a -> a -> V3 a
spherical dir elev = V3 (sin dir * cos elev) (-sin elev) (-cos dir * cos elev)

facing :: V3 Int -> V3 Float -> V3 Float -> V3 Float -> [(Float, (V3 Int, V3 Int))]
facing b p dir n = [(norm (p - m), (b, fmap floor n))
  | let nd = dot dir n
  , nd < 0
  , let m = p - dir ^* (abs (dot (b' + n ^* 0.5 - p) n) / nd)
  , F.all ((<=0.50001) . abs) (m - b')
  ]
  where
    b' = fmap fromIntegral b

(.^) :: (Elevate f g, MonadIO m) => Instance g m -> f a -> m a
i .^ f = i .- elevate f

(.&) :: (MonadIO m) => Instance (Public s t) m -> StateT s m a -> m a
i .& m = do
  s <- i .- Stateful get
  (a, s') <- runStateT m s
  i .- Stateful (put s')
  return a

data Public s t a = Operate (t a) | Stateful (State s a)

instance Tower (Public s t) where
  type Floors (Public s t) = '[State s, t]
  stairs = Stateful `rung` Operate `rung` Nil

sharing :: Monad m => (forall x. t x -> StateT s m x) -> s -> Object (Public s t) m
sharing h s = Object $ \case
  Stateful m -> let (a, s') = runState m s in return (a, sharing h s')
  Operate t -> liftM (fmap (sharing h)) $ runStateT (h t) s
