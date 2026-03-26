<div align="center"><h1> PCVS -- Benchmarks </h1></div>

Curated list of test suites and benchmarks adapted to be used with [PCVS].

## How to use this repository

To use this repository, you will [PCVS] installed and configured.
For more information about the system, please refer to its [documentation](https://cea-hpc.github.io/pcvs).

> [!NOTE]
> pcvs-core is a submodule pointing to the latest version of [PCVS].
> You can use it to install the system.

PCVS relies on two sets of configuration: a profile you should have and test descriptions are provided
in this repository.\
Running tests should be as simple as launching

```console
pcvs run -p <your/profile> <path/to/the/suite>
```

> [!TIP]
> If the configuration of the tests parameters does not correspond to your expectations,
> take a look to the `pcvs.setup` of the suite.

## How to find a test-suite

The repository is organized as follow

- **Accelerator**: Tests for accelerators (CUDA, HIP...)
- **Applications**: Mini-app known to be synthetic
- **Hybrid**: NAS multi-zone benchmarks
- **MPI**: MPI specific tests and benchmarks
- **OpenMP**: OpenMP specific tests and benchmarks
- **Performance**: Raw performance benchmarks (OSU only for now)
- **Serial**: NAS serial benchmarks
- **Threads**: Multi-threading specific tests and benchmarks (`pthread`, `std::thread`...)

## License

All benchmarks and tests redistributed in this repository are licensed under an open-source license.\
See [LISTINGS](LISTINGS) for more information on a particular one.

<!-- Links -->

[pcvs]: https://github.com/cea-hpc/pcvs
