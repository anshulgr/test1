module Shapes.Sphere where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Shapes.Polygon

circlePoints radius x y z number=[let alpha = twoPi * i /number
                                in (radius*(sin (alpha)) ,radius * (cos (alpha)),0)|i <-[1,2..number]]
       where
         twoPi = 2*pi


circle radius x y z = circlePoints radius x y z 100




renderCircle r x y z = displayPoints (circle r x y z) LineLoop
fillCircle r x y z= displayPoints (circle r x y z) Polygon

{-
main = renderInWindow $ do
                        clear [ColorBuffer]
                        renderCircleApprox 0.8 10
-}
sphereFound (y:xs) = let myPoints   = (returnArgsPoly 1 (y:xs))
                         radius     = (read (eatSpaces(head xs))::GLfloat)
                       in hOpenGlCircle radius myPoints 





{-
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints =[(-0.25, 0.25, 0.0)
          ,(0.75, 0.35, 0.0)
          ,(0.75, -0.15, 0.0)
          ,((-0.75), -0.25, 0.0)]

-}
makeVertexes :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)

renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertexes ps






displayPoints points primitiveShape = do
                        renderAs primitiveShape points
                        flush


renderInWindow displayFunction = do
             (progName,_) <- getArgsAndInitialize
             createWindow progName
             displayCallback $= displayFunction
             mainLoop

{-
createAWindow windowName = do
           createWindow windowName
           displayCallback $= display

-}
display = clear [ColorBuffer]




hOpenGlCircle radius myPoints = let x = getCord 1 (head myPoints)
				    y = getCord 2 (head myPoints)
				    z = getCord 3 (head myPoints)
				      in fillCircle radius x y z

--getCord::Int->[(GFloat,GFloat,GFloat)]->GFloat
getCord n (x,y,z)
		| n==1 = x	
		| n==2 = y
		| n==3 = z





setPointOfView pPos = do
                      (alpha,beta,r) <- get pPos
                      let 
                        (x,y,z)= calculatePointOfView alpha beta r
                        (x2,y2,z2) = calculatePointOfView ((alpha+90)`mod` 360) beta r
                            in lookAt (Vertex3 x y z) (Vertex3 0 0 0) (Vector3 x2 y2 z2)


calculatePointOfView alp bet r =do
                                let alpha = fromIntegral alp*2*pi/fromIntegral 360
                                    beta = fromIntegral bet*2*pi/fromIntegral 360
                                    y = r * cos alpha
                                    u = r * sin alpha
                                    x = u * cos beta
                                    z = u * sin beta
                                  in (x,y,z)


















reshape screenSize@(Size w h) = do
  viewport $= ((Position 0 0), screenSize)
  matrixMode $= Projection
  loadIdentity
  let near= 0.001
      far= 40
      fov= 90
      ang= (fov*pi)/(360)
      top= near / ( cos(ang) / sin(ang) )
      aspect = fromIntegral(w)/fromIntegral(h)
      right = top*aspect
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

































