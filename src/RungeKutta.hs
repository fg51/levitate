module RungeKutta (rk4, rungekuttaLoop) where

--import Data.List (unfoldr)

type RungeFunc = (Double -> Double -> Double -> Double)
type RungeValues = (Double, Double, Double)



rungekuttaLoop :: RungeFunc -> RungeFunc
    -> Double -> Double -> RungeValues
    -> Maybe (RungeValues, RungeValues)
rungekuttaLoop f g tEnd dt txy @ (t, x, y)
    | t <= tEnd = Just (txy, rk4 f g dt txy)
    | otherwise = Nothing



rk4 :: RungeFunc -> RungeFunc -> Double -> RungeValues
    -> RungeValues
rk4 f g dt (t, x, y) =
        ( t + dt
        , x + (k1x + 2 * k2x + 2 * k3x + k4x) / 6
        , y + (k1y + 2 * k2y + 2 * k3y + k4y) / 6
        )
  where
    k1x = dt * (f t x y)
    k1y = dt * (g t x y)

    k2x = dt * (f (t + dt / 2) (x + k1x / 2) (y + k1y / 2))
    k2y = dt * (g (t + dt / 2) (x + k1x / 2) (y + k1y / 2))

    k3x = dt * (f (t + dt / 2) (x + k2x / 2) (y + k2y / 2))
    k3y = dt * (g (t + dt / 2) (x + k2x / 2) (y + k2y / 2))

    k4x = dt * (f (t + dt) (x + k3x) (y + k3y))
    k4y = dt * (g (t + dt) (x + k3x) (y + k3y))
