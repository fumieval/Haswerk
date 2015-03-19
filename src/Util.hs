module Util where
import BurningPrelude
import Control.Object
import Control.Elevator
import Control.Monad.Catch
import qualified Data.Map as Map

play :: Metric f => Float -> f Float -> f Float
play t v
  | quadrance v < t*t = zero
  | otherwise = v

accel :: V2 Float -> V2 Float
accel v = v ^* quadrance v ** 0.3

spherical :: RealFloat a => a -> a -> V3 a
spherical dir elev = V3 (sin dir * cos elev) (-sin elev) (-cos dir * cos elev)

spherical' :: RealFloat a => V2 a -> V3 a
spherical' (V2 dir elev) = spherical dir elev

(.^) :: (MonadIO m, MonadMask m) => Instance (Public s t) m -> t a -> m a
i .^ f = i .- Operate f
infixr 3 .^

(.&) :: (MonadIO m, MonadMask m) => Instance (Public s t) m -> StateT s m a -> m a
i .& m = do
  s <- i .- Stateful get
  (a, s') <- runStateT m s
  i .- Stateful (put s')
  return a

infixr 3 .&

data Public s t a = Operate (t a) | Stateful (State s a)

instance Tower (Public s t) where
  type Floors (Public s t) = '[State s, t]
  stairs = Stateful `rung` Operate `rung` Nil

sharing :: Monad m => (forall x. t x -> StateT s m x) -> s -> Object (Public s t) m
sharing h s = Object $ \case
  Stateful m -> let (a, s') = runState m s in return (a, sharing h s')
  Operate t -> liftM (fmap (sharing h)) $ runStateT (h t) s
