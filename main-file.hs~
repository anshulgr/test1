import Parser
import Shapes.Polygon
import Shapes.Triangle
import Shapes.Torus
import Shapes.Cylinder
import Shapes.Cone
import Shapes.Sphere
import Appearence.Camera
{-import Parser
import Appearence
import Shapes-}
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Environment


createAWindow windowName arg = do
           createWindow windowName
           displayCallback $= separateResult arg


separateResult arg = do
          clear [ColorBuffer,DepthBuffer]
          --currentColor $= Color4 1 0 0 0
          res <- mainComputation arg
         -- error (show res)
          case res of
            Right x -> displayFunction x
            Left x  -> error (show x)


displayFunction [] = flush
displayFunction (x:xs) = do
         
          case (head x) of
                             "polygon"  -> polygonFound (tail x)
                             "triangle" -> triangleFound (tail x)
                             "cylinder" -> cylinderFound (tail x)
                             "cone"     -> coneFound (tail x)
                             "torus"    -> torusFound (tail x)
                             "sphere"   -> sphereFound (tail x)
                             "camera"   -> cameraFound (tail x)
                            
                             
                             res        -> defaultFunc  
         -- flush
          displayFunction xs

defaultFunc = print ([(0.0,0.0,0.0)])
main = do
    (arg1:args) <- getArgs
    (progName,_) <-getArgsAndInitialize
    createAWindow progName arg1
    mainLoop

