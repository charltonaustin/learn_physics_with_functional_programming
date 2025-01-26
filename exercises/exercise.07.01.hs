import Graphics.Gnuplot.Simple

main :: IO ()
main = do
    let xValues :: [Double]
        xValues = [-10.0, -9.99 .. 10.0]
    (plotFunc [] xValues sin)
