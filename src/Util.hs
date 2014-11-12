module Util where
import Data.Foldable as F
import Linear

play :: Metric f => Float -> f Float -> f Float
play t v
  | quadrance v < t*t = zero
  | otherwise = v

accel :: V2 Float -> V2 Float
accel v = v ^* quadrance v ** 0.5

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
