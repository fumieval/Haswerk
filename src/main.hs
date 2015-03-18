import BurningPrelude
import Call
import qualified Player
import World
import Render
import Util
import Assets
import Voxel
import Control.Lens
import Control.Elevator
import qualified Block
import qualified Data.Heap as Heap
import Debug.Trace
import Entity
main = runCallDefault $ do
  setFPS 30
  disableCursor
  -- clearColor (V4 0 0 0 0)
  world <- new $ variable newWorld
  pl <- new $ Player.object @>>^ (world.-)
  prevCursor <- new $ variable zero

  linkMouse $ \case
    Button (Down 0) -> pl .^ Player.Attack
    Button (Down 1) -> pl .^ Player.Act
    Cursor pos -> do
      pos' <- prevCursor .- get
      pl .^ Player.Turn ((pos - pos') / 300)
      prevCursor .- put pos
    _ -> return ()

  linkPicture $ \_ -> return $ translate (V2 320 240) $ bitmap _crosshair_png

  linkGraphic $ \dt -> do
    pl .^ Player.Update dt

    dir <- new $ variable zero

    whenM (keyPress KeyW) $ dir .- id += V2 0 1
    whenM (keyPress KeyS) $ dir .- id -= V2 0 1

    whenM (keyPress KeyA) $ dir .- id -= V2 1 0
    whenM (keyPress KeyD) $ dir .- id += V2 1 0

    v <- dir .- get

    pl .^ Player.Move (v ^* dt)

    psp <- pl .^ Player.GetPerspective
    pos <- pl .& use Player.position

    ray <- pl .& uses Player.angleP spherical'

    let mk i p s = let n = fromSurface s in
          case penetration ray (p + n ^* 0.5 - pos) n of
            Just k -> Heap.singleton $ Heap.Entry k (i, s)
            Nothing -> Heap.empty

    (sceneB, focusB) <- world .- do
      zoom blocks $ iapprises Block.Render
        (\(i, ss) a -> let p = fmap fromIntegral i in (translate p (foldMap a ss)
        , flip foldMap ss $ mk i p))
        mempty

    case fmap (Heap.payload . fst) $ Heap.uncons focusB of
      Just (i, s) -> pl .& Player.currentTarget .= TBlock i s
      Nothing -> pl .& Player.currentTarget .= TNone

    return $ psp (translate pos skybox <> sceneB)

  stand

-- check whether the given ray passes through a 1*1 square
penetration :: V3 Float -> V3 Float -> V3 Float -> Maybe Float
penetration v p n = do
  let c = dot v n
  let ob = dot p n
  let k = ob / c
  guard (c < 0)
  guard $ all (<=0.50001) $ abs $ p - k *^ v
  return k
