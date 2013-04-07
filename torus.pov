torus {
0.1
0.4
  pigment { color rgb <1,1,0> }
  scale <1.5,1,1>
  rotate <-45,0,0>
  translate <0,2,0>
}

light_source {
  <0,-10,-10>
  color rgb <1,1,1>
}

// first, the camera position
camera {
  location <2,5,-10>
  look_at <0,0,0>
}
