import Graphics.Gnuplot.Simple

type R = Double

xRange :: [R]
xRange = [-3, -2.9 .. 3]

fn :: R -> R
fn x = x**2

plot :: IO()
plot = plotFunc [ Title "y = x^2"
                , XLabel "X"
                , YLabel "Y"
                , PNG "first_plot.png"
                , Key Nothing
                ] xRange fn
