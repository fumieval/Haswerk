{-# LANGUAGE DeriveFunctor #-}
module Block where
import Voxel
import Assets
import BurningPrelude
import Graphics.Holz
import Control.Object

type Appearance = (Prop, Cube Prop -> V3 Float -> [Vertex])

type Block = Mortal Action IO ()

data Prop = Transparent | Opaque

data Action x where
  Render :: Float -> Action Appearance
  Damage :: Float -> Action ()

genStrip :: [a] -> [a]
genStrip (x : y : zs) = go x y zs where
  go a b (c : cs) = a : b : c : go c b cs
  go _ _ [] = []

cubeMesh :: Cube [V2 Float] -> V3 Float -> Cube [Vertex]
cubeMesh uv pos = tabulate $ \s -> genStrip
  $ zipWith (\t v -> Vertex (pos + v) t (fromSurface s)) (index uv s) (cubePoints s)

cubePoints :: Surface -> [V3 Float]
cubePoints SRear = [V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5)]
cubePoints SFront = [V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) (-0.5) 0.5]
cubePoints SLeft = [V3 (-0.5) 0.5 0.5, V3 (-0.5) 0.5 (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) (-0.5) (-0.5)]
cubePoints SRight = [V3 0.5 0.5 (-0.5), V3 0.5 0.5 0.5, V3 0.5 (-0.5) (-0.5), V3 0.5 (-0.5) 0.5]
cubePoints STop = [V3 0.5 0.5 (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 0.5, V3 (-0.5) 0.5 0.5]
cubePoints SBottom = [V3 (-0.5) (-0.5) (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5]

hidden :: Monoid a => Prop -> a -> a
hidden Transparent a = a
hidden Opaque _ = mempty

texUV :: Int -> Int -> [V2 Float]
texUV m n = [V2 u v, V2 u' v, V2 u v', V2 u' v'] where
  u = fromIntegral m / 16
  v = fromIntegral n / 16
  u' = u + 1/16 - 2/256
  v' = v + 1/16 - 2/256

cubeMeshOn :: Cube [V2 Float] -> Cube Prop -> V3 Float -> [Vertex]
cubeMeshOn uv c pos = fold $ hidden <$> c <*> cubeMesh uv pos

dirt :: Appearance
dirt = (Opaque, cubeMeshOn $ pure (texUV 2 0))

gdirt :: Appearance
gdirt = (Opaque, cubeMeshOn c) where
  c = Cube (texUV 0 0) (texUV 2 0) (texUV 3 0) (texUV 3 0) (texUV 3 0) (texUV 3 0)

stoneBrick :: Appearance
stoneBrick = (Opaque, cubeMeshOn $ pure (texUV 6 3))