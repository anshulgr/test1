module Shapes.Torus where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Appearence.Color
import Appearence.Translate
import Appearence.Rotate
import Appearence.Scale
import Data.List


torusFound::[String] -> IO ()
torusFound (x:y:xs)= let rad1 = read x
			 rad2 = read y
		            in parseRTorus xs rad1 rad2

parseRTorus [] r h= do currentColor $= Color4 1 0 0 0
                       hOpenGlTorus r h
parseRTorus (xs:xss) r h = do
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
                                                 parseRTorus xss r h
                             hOpenGlTorus r h


hOpenGlTorus rad1 rad2 = renderObject Solid (Torus rad1 rad2 100 100)
