module Geometry where
import Linear
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

perlin :: (RealFloat a, Hashable a) => Int -> V2 a -> a
perlin n pos = lp v (lp u (grad (V2 0 0)) (grad (V2 1 0))) (lp u (grad (V2 0 1)) (grad (V2 1 1))) where
  grid = fmap floor pos :: V2 Int
  c@(V2 u v) = pos - fmap fromIntegral grid
  spline x = x ^ 2 * (3 - 2 * x)
  grad i = dot (c - fmap fromIntegral i) $ angle $ fromIntegral $ hashWithSalt n (grid + i)
  lp t a b = a + t * (b - a)
