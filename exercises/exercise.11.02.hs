import Graphics.Gnuplot.Simple

type R = Double

xRange :: [R]
xRange = [0, 0.1 .. 10.0]

plot :: IO()
plot = plotFuncs [ Title "Sin and Cos"
                 , XLabel "X"
                 , YLabel "Y"
                 , PNG "second_plot.png"
                 , Key Nothing
                 ] xRange [cos, sin]
