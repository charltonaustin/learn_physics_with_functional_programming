import SimpleVec (iHat, kHat, xComp, zComp, projectilePos, (^+^), (*^) )
import Graphics.Gnuplot.Simple (Attribute(..), plotPath)

main :: IO()
main =
  let posInitial = 10 *^ kHat
      velInitial = 20 *^ cos (pi/6) *^ iHat ^+^ 20 *^ sin (pi/6) *^ kHat
      posFun = projectilePos posInitial velInitial
      pairs = [(xComp r, zComp r) | t <- [0, 0.1 ..], let r = posFun t]
      plottingPairs = takeWhile (\(_, z) -> z >= 0) pairs
  in plotPath [Title "Projectile Motion"
              ,XLabel "Horizontal Position (m)"
              ,YLabel "Verticle Position (m)"
              ,PNG "projectile.png"
              ,Key Nothing
              ] plottingPairs
