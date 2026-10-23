# Parallelism (PAR)

The `parallel` stdlib module's list `pmapWith`, in C++ (std::thread),
Python (multiprocessing) and Rust (std::thread), on the workloads of
`parallel.bench`: element i costs `spin(base)` iterations. Those workloads
carry `Int` elements, 16 bytes each on the wire (ENC-2), so PAR-2's
overhead includes encoding them; a fixed-width variant of the workload
belongs in the stdlib bench.

Scaling is bounded by the host. The reference host (AMD Ryzen 5 7535U, 6
cores / 12 threads, power-limited) reaches about 4-5x on pure Python work
at 8-12 processes whatever the scheduler. A scaling measurement on a loaded
machine measures the load.

### PAR-1 Time falls as 1/w up to the core count
Intent: proposed
Bench: par-scaling
Model: t(w) = a + B/w for w <= physical cores; a = start, splitting and gathering; B = the sequential work
Measured: pending
Limit: cpp.speedup8 >= 3 x, rs.speedup8 >= 3 x, py.speedup8 >= 2.5 x

### PAR-2 Each element costs a fixed dispatch overhead over sequential map
Intent: proposed
Bench: par-scaling
Model: t(n) = c + b*n with zero-cost elements; overhead = b(par, 4 workers) - b(seq)
Measured: pending
Limit: cpp.overhead <= 200 ns, rs.overhead <= 200 ns, py.overhead <= 5000 ns

The overhead decides the smallest element cost worth parallelizing: an
element must cost several times the overhead for `pmap` to pay off.
