entry sum_mat [m][n] (a: [m][n]f32) : f32 =
  f32.sum (flatten a)

entry add_scalar [m][n] (c: f32) (a: [m][n]f32) : [m][n]f32 =
  map (map (+c)) a
