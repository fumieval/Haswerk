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
import qualified Audiovisual.Text as Text
import Text.Printf

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

  text <- Text.simple defaultFont 24

  linkPicture $ \_ -> return $ translate (V2 320 240) $ bitmap _crosshair_png

  linkPicture $ \_ -> do
    t <- getFPS

    return $ mconcat [translate (V2 40 40) $ text $ printf "%.1f" t]

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
            Just k -> Heap.singleton $! Heap.Entry k (i, s)
            Nothing -> Heap.empty

    (sceneB, focusB) <- world .- do
      zoom blocks $ flip (iapprises (Block.Render dt)) mempty $ \(i, ss) f ->
        let !p = fmap fromIntegral i
        in (translate p (foldMap f ss), foldMap (mk i p) ss)

    case fmap (Heap.payload . fst) $ Heap.uncons focusB of
      Just (i, s) -> pl .& Player.currentTarget .= TBlock i s
      Nothing -> pl .& Player.currentTarget .= TNone

    return $ psp (translate pos skybox <> sceneB)

  stand

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
