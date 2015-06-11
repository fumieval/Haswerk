module Geometry where
import BurningPrelude
import Data.Hashable

spherical :: RealFloat a => a -> a -> V3 a
spherical dir elev = V3 (sin dir * cos elev) (-sin elev) (-cos dir * cos elev)

spherical' :: RealFloat a => V2 a -> V3 a
spherical' (V2 dir elev) = spherical dir elev

-- check whether the given ray passes through a 1*1 square
penetration :: V3 Float -> V3 Float -> V3 Float -> Maybe Float
penetration v p n
  | c < 0
  , quadrance p < 8^2
  , all (<=0.50001) $ abs $ p - k *^ v = Just k
  | otherwise = Nothing
  where
    c = dot v n
    ob = dot p n
    k = ob / c

perlin :: (Floating a, Hashable a) => Int -> V2 a -> a
perlin n pos = lp sy (lp sx (grad 0) (grad 1)) (lp sx (grad 2) (grad 3)) where
  spline x = x ^ 2 * (3 - 2 * x)
  V2 sx sy = fmap spline v
  grad i = dot v $ angle (fromIntegral (hashWithSalt (n + i) v))
  lp t a b = t * a + (1 - t) * b
  v = fmap (\x -> 1 / (1 + exp x)) pos