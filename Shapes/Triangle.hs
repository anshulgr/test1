module Shapes.Triangle where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Appearence.Color
import Appearence.Translate
import Appearence.Rotate
import Appearence.Scale
import Data.List


triangleFound::[String] -> IO ()
triangleFound (x:xs)= let myPoints =  (returnArgsTri 3 (x:xs))
                                          in  parseRTriangle xs myPoints

					
parseRTriangle [] myPoints= do currentColor $= Color4 1 0 0 0
                               hOpenGlTriangle myPoints
parseRTriangle (xs:xss) myPoints= do
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
                                                  parseRTriangle xss myPoints
                               hOpenGlTriangle myPoints



returnArgsTri::Int ->[String] -> [(GLfloat,GLfloat,GLfloat)]
returnArgsTri 0 _ = []
returnArgsTri a [] = []
returnArgsTri a (x:xs) = (returnArgTri x):(returnArgsTri (a-1) xs)

myFunc1::String -> (String,String)
myFunc1 [] =([],[])
myFunc1 (x:xs) = 
                if ((x == ',') || (x == '>'))
                 then ([],xs)
                 else
                  if (isSpace x)
                    then (fst(myFunc1 xs),snd(myFunc1 xs))
                    else (x:fst(myFunc1 xs),snd(myFunc1 xs))

returnArgTri :: String -> (GLfloat,GLfloat,GLfloat)
returnArgTri [] = (0.0,0.0,0.0)
returnArgTri (x:xs) = if x== '<'
                         then let (y1,z1) = (myFunc1 xs)
                               in let (y2,z2 ) = (myFunc1 z1)
                                    in let (y3,z3 )= (myFunc1 z2)
                                         in ((read y1) , (read y2) , (read y3)) 
                         else returnArgTri xs

hOpenGlTriangle args= renderPrimitive Triangles $mapM_ (\(x, y, z)->vertex $ Vertex3 x y z) args

