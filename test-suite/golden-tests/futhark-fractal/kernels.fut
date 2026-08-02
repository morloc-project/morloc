-- Mandelbrot escape-time render. Every pixel is independent, so the whole
-- image is a single data-parallel map -- the case Futhark exists for. The h/w
-- value parameters determine the result shape; the generated glue reads the
-- runtime dimensions back when it reconstructs the host matrix.
entry render (h: i64) (w: i64) (cx: f64) (cy: f64) (scale: f64) (max_iter: i32)
  : [h][w]i32 =
  tabulate_2d h w (\i j ->
    let re0 = cx + (f64.i64 j - f64.i64 w / 2f64) * scale
    let im0 = cy + (f64.i64 i - f64.i64 h / 2f64) * scale
    let (_, _, count) =
      loop (zr, zi, k) = (0f64, 0f64, 0i32)
      while k < max_iter && zr * zr + zi * zi <= 4f64 do
        (zr * zr - zi * zi + re0, 2f64 * zr * zi + im0, k + 1i32)
    in count)
