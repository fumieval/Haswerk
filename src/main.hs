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

pipeline :: Address (State w) (System s) -> Object e (StateT w (System s)) -> Object e (System s)
pipeline addr obj = Object $ \e -> do
  s <- addr .& get
  ((a, o'), s') <- runStateT (runObject obj e) s
  addr .& put s'
  return (a, pipeline addr o')

main = runSystemDefault $ do
  setFPS 30
  miku <- liftIO $ fromPMD "C:/Users/Fumiaki/Documents/Lat_miku/Normal.pmd"
  world <- new $ variable newWorld
  pl <- new $ pipeline world Player.object
  
  newJoypad $ liftO $ accept $ \case
    PadButton _ (Down 7) -> pl .^> Player.Attack
    PadButton _ (Down 5) -> pl .^> Player.Act
    PadButton _ (Down 4) -> pl .& Player.position' -= V3 0 1 0
    PadButton _ (Down 6) -> pl .& Player.position' += V3 0 1 0
    PadButton _ (Down i) -> liftIO $ print i
    _ -> return ()
  
  newGraphic $ liftO $ accept $ \dt -> do
    pl .^> Player.Update dt
    pos <- pl .& use Player.position
    psp <- pl .^> Player.GetPerspective

    (p:_) <- getGamepads
    (mx : mz : px : py : _) <- gamepadAxes p

    pl .^> Player.Move (play 0.01 $ V2 mx mz ^* dt)
    pl .^> Player.Turn (play 0.01 $ V2 px py ^* dt)
    
    w <- world .& get
    return $ mconcat
      [ psp $ mconcat [renderWorld pos w, miku]
      , crosshair]
  
  stand