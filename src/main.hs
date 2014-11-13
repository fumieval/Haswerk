{-# LANGUAGE TemplateHaskell, LambdaCase, Rank2Types #-}
import Call
import qualified Player
import World
import Render
import Control.Monad.Trans
import Util
import Control.Monad.State.Strict
import Data.Monoid
import Assets
import Util
import Control.Lens

main = runSystemDefault $ do
  setFPS 30
  world <- new $ variable newWorld
  pl <- new $ pipeline world Player.object
  
  linkGamepad $ \case
    PadButton _ (Down 7) -> pl .^ Player.Attack
    PadButton _ (Down 5) -> pl .^ Player.Act
    _ -> return ()
  
  linkGraphic $ \dt -> do
    pl .^ Player.Update dt
    pos <- pl .& use Player.position
    psp <- pl .^ Player.GetPerspective

    (p:_) <- getGamepads
    (mx : mz : px : py : _) <- gamepadAxes p
    bts <- gamepadButtons p

    when (bts !! 4) $ pl .& Player.position' += V3 0 (3*dt) 0
    when (bts !! 6) $ pl .& Player.position' -= V3 0 (3*dt) 0

    pl .^ Player.Move (play 0.002 $ V2 mx mz ^* dt)
    pl .^ Player.Turn (play 0.002 $ V2 px py ^* dt)
    
    w <- world .& get
    return $ mconcat
      [ psp $ renderWorld pos w
      , crosshair]
  
  stand