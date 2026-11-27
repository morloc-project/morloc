-- Tuple I/O across the Futhark boundary. Futhark represents each tuple as a
-- single opaque value (it does not flatten it), so these exercise the opaque
-- construction path (tuple arguments) and the projection path (tuple returns),
-- over scalar, vector, and matrix fields with distinct, order-sensitive types.

-- tuple argument (scalar + vector) reduced to a scalar
entry weighted_sum (p: (f32, []f32)) : f32 =
  let (w, xs) = p in f32.sum (map (\x -> w * x) xs)

-- plain scalar + vector arguments producing a heterogeneous, order-sensitive
-- tuple return (i32, f32, []f32): a field transposition would corrupt the result
entry tagged (k: i32) (xs: []f32) : (i32, f32, []f32) =
  (k, f32.sum xs, map (\x -> x * 2f32) xs)

-- tuple argument carrying a matrix (rank 2) + a scalar, reduced in Futhark
entry mat_reduce (p: ([][]f32, f32)) : f32 =
  let (m, s) = p in f32.sum (flatten m) + s
