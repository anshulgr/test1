module Scale where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

getScaleVal :: String->(String,String)
getScaleVal [] = ([],[])
getScaleVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getScaleVal xs),snd(getScaleVal xs))
                      else (x:fst(getScaleVal xs),snd(getScaleVal xs))


scaleImage1 xs =let (r,rs)   = getScaleVal (tail xs)
                    in 
                      let (g, gs) = getScaleVal rs
                        in 
                          let (b,bs) = getScaleVal gs
                            in scale  (read r ::GLfloat) (read g ::GLfloat) (read b ::GLfloat)
                           

