module Util where
import BurningPrelude
import Control.Object
import Control.Elevator

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

(.^) :: (MonadIO m) => Instance (Public s t) m -> t a -> m a
i .^ f = i .- Operate f
infixr 3 .^

(.&) :: (MonadIO m) => Instance (Public s t) m -> StateT s m a -> m a
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

class WitherableWithIndex i t | t -> i where
  iwither :: Applicative f => (i -> a -> f (Maybe b)) -> t a -> f (t b)

instance WitherableWithIndex i (Map.Map i) where
  iwither f = fmap (Map.mapMaybe id) . itraverse f

type WindLike f s a = (a -> f (Maybe a)) -> s -> f s

-- | Send a message to mortals in a container.
apprisesOf :: (Monad m, Monoid r) => WindLike (WriterT r m) s (Mortal f m b)
  -> f a -> (a -> r) -> (b -> r) -> StateT s m r
apprisesOf l f p q = StateT $ \t -> do
  (t', res) <- runWriterT $ flip l t
    $ \obj -> lift (runEitherT $ runMortal obj f) >>= \case
      Left r -> writer (Nothing, q r)
      Right (x, obj') -> writer (Just obj', p x)
  return (res, t')

type IndexedWindLike i f s a = (i -> a -> f (Maybe a)) -> s -> f s

-- | Send a message to mortals in a container.
iapprisesOf :: (Monad m, Monoid r) => IndexedWindLike i (WriterT r m) s (Mortal f m b)
  -> f a -> (i -> a -> r) -> (i -> b -> r) -> StateT s m r
iapprisesOf l f p q = StateT $ \t -> do
  (t', res) <- runWriterT $ flip l t
    $ \i obj -> lift (runEitherT $ runMortal obj f) >>= \case
      Left r -> writer (Nothing, q i r)
      Right (x, obj') -> writer (Just obj', p i x)
  return (res, t')

-- | Send a message to mortals in a container.
iapprises :: (WitherableWithIndex i t, Monad m, Applicative m, Monoid r) => f a -> (i -> a -> r) -> (i -> b -> r) -> StateT (t (Mortal f m b)) m r
iapprises = iapprisesOf iwither
