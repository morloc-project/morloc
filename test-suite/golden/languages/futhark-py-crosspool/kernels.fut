entry matmul [m][k][n] (a: [m][k]f32) (b: [k][n]f32) : [m][n]f32 =
  map (\arow ->
        map (\bcol -> f32.sum (map2 (*) arow bcol))
            (transpose b))
      a

entry sum_mat [m][n] (a: [m][n]f32) : f32 =
  f32.sum (flatten a)
