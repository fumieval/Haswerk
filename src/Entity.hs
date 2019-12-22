module Entity where

import Lib.Cube
import Linear

data Target = TBlock (V3 Int) Surface
  | TNone
