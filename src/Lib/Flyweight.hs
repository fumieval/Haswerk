{-# LANGUAGE FlexibleContexts #-}
module Lib.Flyweight where
import Prelude.Kai
import Data.Functor.Rep
import qualified Data.HashMap.Strict as HM
import System.Random
import Control.Concurrent
import Data.Hashable
import System.Mem.Weak

data I a = I { hashI :: Int, contentI :: a } deriving Show

instance Eq (I a) where
  I i _ == I j _ = i == j

instance Ord (I a) where
  compare (I i _) (I j _) = compare i j

data Flyweight a b = Flyweight (a -> IO b) (b -> IO ()) (MVar (HM.HashMap Int b))

unique :: a -> IO (I a)
unique a = do
  i <- randomIO
  return (I i a)

hashed :: Hashable a => a -> I a
hashed a = I (hash a) a

edit :: (Hashable (Rep f), Representable f) => f (a -> b) -> I a -> f (I b)
edit f (I i a) = tabulate $ \p -> I (hashWithSalt i p) (index f p a)

fetch :: Flyweight a b -> I a -> IO b
fetch (Flyweight f g tv) k@(I i a) = modifyMVar tv $ \m -> case HM.lookup i m of
  Just b -> return (m, b)
  Nothing -> do
    b <- f a
    addFinalizer k $ do
      g b
      modifyMVar_ tv (return . HM.delete i)
    return (HM.insert i b m, b)

newFlyweight :: (a -> IO b) -> (b -> IO ()) -> IO (Flyweight a b)
newFlyweight f g = Flyweight f g <$> newMVar HM.empty