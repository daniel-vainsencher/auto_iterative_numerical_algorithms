{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IterativeNumerical (test, sgd) where


{- 
To incorporate:
Optimization in R^n (rather than R), using:
- http://dis.um.es/~alberto/hmatrix/examples.html
with examples from 
- https://github.com/mikeizbicki/HLearn
or alternately  
- https://github.com/wellposed/numerical/tree/master/src/Numerical/Array
if that gets some docs/examples 
 -}
import Control.Auto                 -- the main entry point
import Prelude hiding ((.), id) 
import Data.Serialize

type SGD m = Auto m Double Double

newtype Location = Location Double
  deriving (Num, Fractional, Serialize, Show)

newtype Gradient = Gradient Location
  deriving (Num, Fractional, Show)

type GradientFunction = Location -> Gradient

sgd :: Location -> Auto' GradientFunction Location
sgd start = proc gf -> do
   rec let Gradient loc_change = - 0.1 * (gf x)
{- Should have variable step sizes, haven't made that work yet.
       step_size = 1.0 / t
       step_size <- id -< arr id [1 ..] -}
       x <- sumFromD start -< loc_change
   id -< x

test = let gradienter_source = repeat $ \x -> Gradient $ 2*x
           locations = streamAuto' (sgd 5) $ take 100 $ gradienter_source
       in do putStrLn $ show locations
