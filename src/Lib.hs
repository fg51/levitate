module Lib
    ( someFunc
    ) where

import Data.List (unfoldr)

import RungeKutta (rungekuttaLoop, rk4)
import ConvertJson (toJsonString)

someFunc :: IO ()
someFunc = do
    -- putStrLn "someFunc"
    putStrLn $ toJsonString $ unfoldr rkloop tvy0
      where
        dt = 0.01
        tEnd = 1
        rkloop = rungekuttaLoop calAccel calVelocity tEnd dt
        tvy0 = (0, 0, 0)



calAccel t v y = -10

calVelocity t v y = v
