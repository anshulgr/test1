module Shapes.Cone where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Shapes.Cylinder
import Shapes.Polygon
import Appearence.Translate
import Appearence.Color
import Appearence.Rotate
import Appearence.Scale
import Data.List
                         
coneFound::[String] -> IO ()
coneFound (x:r1:y:r2) = let point1 = (returnArgsCy 1 [x])
                            rad    =  read (eatSpaces (r1))
                            point2 = (returnArgsCy 1 [y])
					                   in  parseRCone r2 rad (calcHeightCone (point1++point2))
					                    --in print (point1++point2)

calcHeightCone [(x1,y1,z1),(x2,y2,z2)]= sqrt (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2))+((z1-z2)*(z1-z2)))
					
parseRCone [] r h= do currentColor $= Color4 1 0 0 0
                      hOpenGlCone r h
parseRCone (xs:xss) r h = do
                             if (isInfixOf "color rgb" xs )
                              then
                               let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                in
                                  setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                              else
                               
                                       if (isInfixOf "translate" xs )
                                         then
                                           let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                            in
                                              translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                         else
                                           if (isInfixOf "rotate" xs )
                                             then
                                               let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                in
                                                  rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                             else
                                               if (isInfixOf "scale" xs )
                                               then
                                                 let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                  in
                                                   scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                               else
                                                  parseRCone xss r h
                             hOpenGlCone r h



hOpenGlCone r h= renderObject Solid (Cone r h 1000 1000)

