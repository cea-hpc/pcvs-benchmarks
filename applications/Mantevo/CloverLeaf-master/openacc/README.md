# CloverLeaf_OpenACC

This is the OpenACC version of CloverLeaf version 1.3. 

## Release Notes

### Version 1.3

CloverLeaf 1.3 contains a number of optimisations over previous releases.
These include a number of loop fusion optimisations and the use of scalar variables over work arrays.
Overall this improves cache efficiency.

This OpenACC version is based off of the CAPS Kernels version.

Due to limitations in data movement for Fortran data types, this version does not currently support tiling.


## Note

The two other OpenACC repos for KERNELS and LOOPS have been marked as deprecated, this version should be considered the reference OpenACC version.

## Performance

Expected performance is give below.

If you do not see this performance, or you see variability, then is it recommended that you check MPI task placement and OpenMP thread affinities, because it is essential these are pinned and placed optimally to obtain best performance.

Note that performance can depend on compiler (brand and release), memory speed, system settings (e.g. turbo, huge pages), system load etc. 

### Performance Table

| Test Problem  | Time                         | Time                        | Time                        |
| ------------- |:----------------------------:|:---------------------------:|:---------------------------:|
| Hardware      |  E5-2670 0 @ 2.60GHz Core    | E5-2670 0 @ 2.60GHz Node    | E5-2698 v3 @ 2.30GHz Node   |
| Options       |  make COMPILER=INTEL         | make COMPILER=INTEL         | make COMPILER=CRAY          |
| Options       |  mpirun -np 1                | mpirun -np 16               | aprun -n4 -N4 -d8           |
| 2             | 20.0                         | 2.5                         | 0.9                         |
| 3             | 960.0                        | 100.0                       |                             |
| 4             | 460.0                        | 40.0                        | 23.44                       |
| 5             | 13000.0                      | 1700.0                      |                             |

### Weak Scaling - Test 4

| Node Count | Time         |
| ---------- |:------------:|
| 1          |   40.0       |
| 2          |              |
| 4          |              |
| 8          |              |
| 16         |              |


### Strong Scaling - Test 5

| Node Count | Time          | Speed Up |
| ---------- |:-------------:|:--------:|
| 1          |   1700        |  1.0     |
| 2          |               |          |
| 4          |               |          |
| 8          |               |          |
| 16         |               |          |


