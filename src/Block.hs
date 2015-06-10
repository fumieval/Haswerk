{-# LANGUAGE DeriveFunctor #-}
module Block where
import Voxel
import Assets
import BurningPrelude
import Graphics.Holz
import Control.Object

type Block = Mortal Action IO ()

data Prop = Transparent | Opaque

data Action x where
  Render :: Float -> Action (Prop, Cube Prop -> [Vertex])
  Damage :: Float -> Action ()

genStrip :: [a] -> [a]
genStrip (x : y : zs) = go x y zs where
  go a b (c : cs) = a : b : c : go c b cs
  go _ _ [] = []

cubeMesh :: Cube [V2 Float] -> Cube [Vertex]
cubeMesh uv = tabulate $ \s -> zipWith (\t v -> Vertex v t (fromSurface s)) (index uv s) (vs s) where
  vs s = map (cross (fromSurface s ^/ 2)) [V3 (-1) (-1) 1, V3 (-1) 1 1, V3 1 (-1) 1, V3 1 1 1]

hidden :: Monoid a => Prop -> a -> a
hidden Transparent a = a
hidden Opaque _ = mempty

texUV :: Int -> Int -> [V2 Float]
texUV m n = [V2 u v, V2 u' v, V2 u v', V2 u' v'] where
  u = fromIntegral m / 16
  v = fromIntegral n / 16
  u' = u + 1/16
  v' = v + 1/16

cubeMeshOn :: Cube [V2 Float] -> Cube Prop -> [Vertex]
cubeMeshOn uv c = fold $ hidden <$> c <*> cubeMesh uv

dirt :: Block
dirt = mortal $ \case
  Render dt -> return ((Opaque, cubeMeshOn $ pure (texUV 2 0)), dirt)
  Damage d -> left ()

gdirt :: Block
gdirt = mortal $ \case
  Render dt -> return ((Opaque, cubeMeshOn c), gdirt)
  Damage d -> left ()
  where
    c = Cube (texUV 0 0) (texUV 2 0) (texUV 3 0) (texUV 3 0) (texUV 3 0) (texUV 3 0)

stoneBrick :: Block
stoneBrick = mortal $ \case
  Render dt -> return ((Opaque, cubeMeshOn $ pure (texUV 6 3)), stoneBrick)
  Damage d -> left ()
