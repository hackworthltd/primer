# Benchmarking

We run various benchmarks to track performance regressions, measure
simulated network traffic, etc. We try to ensure that the benchmarks
are run in a repeatable, quiescent environment, but we may from time
to time need to change the hardware on which the benchmarks are run,
which will inevitably cause discontinuities in benchmark results. In
the table below, we make a best-effort attempt to track the commits to
`main` after which discontinuities may appear.

| Breaking commit                          | Description
|------------------------------------------|------------------
| [82e1fce] (https://github.com/hackworthltd/primer/commit/82e1fcec836bac7a153014f721578a72140d4cde) | Retired previous benchmark machine. For the time being, benchmarks will be run on a CI cluster, and may be a bit noisier than in the past.
| [d81d43e2](https://github.com/hackworthltd/primer/commit/d81d43e2fe0f466410b0b984aa66ba8e83c19dec) | Retired previous benchmark machine, a dedicated AMD Ryzen 5 3600 6-Core Processor host running only benchmark jobs, and moved subsequent benchmark jobs to a VM running other CI jobs, with an 8-core AMD Ryzen 9 5900HX with Radeon Graphics.
| [d38b9738](https://github.com/hackworthltd/primer/commit/d38b973874bc2491904811567b5da55e70116f54) | Back to previous benchmark machine, as the mixed-use CI machine was not a reliable benchmarking host.
| [04867463](https://github.com/hackworthltd/primer/commit/048674636bbca82a98dd99b9de9561739ea49a48) | Retired previous benchmark machine, and moved subsequent benchmark jobs to an 8-core AMD EPYC 7402P virtual machine.
