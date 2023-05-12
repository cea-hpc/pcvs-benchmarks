# A Simple Consumer For the MPI Meta Iface

Note you need a prepass.dat from the standard:

```
./binding-tool/binding_prepass.py . prepass.dat
```

The `bindingtypes.py` file is directly coming from the MPI standard
documentation to help producing consistent types depending on the target MPI
support.

## mpiiface.py

This is the main consumer class

## mpiheader.py

This is the header generation tool

```
python ./mpiheader.py
```

```c
/*MPI_Win_shared_query*/

/**
 * @brief MPI function MPI_Win_shared_query
 *
 * @param win shared memory window object
 * @param rank rank in the group of window win or MPI_PROC_NULL
 * @param size size of the window segment
 * @param disp_unit local unit size for displacements, in bytes
 * @param baseptr address for load/store access to window segment
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise
 */
int MPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, int *disp_unit, void *baseptr);
int PMPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, int *disp_unit, void *baseptr);
```

## cfbind.py

This generates the Fortran bindings

```
python ./cfbind.py
```

```c
int mpi_win_shared_query_(MPI_Fint* win, int* rank, MPI_Aint *size, int *disp_unit, void *baseptr, int *ierror)
{
/* MPI_Win_shared_query */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_shared_query(c_win, *rank, size, disp_unit, baseptr);


}
int mpi_win_shared_query__(MPI_Win* win, int* rank, MPI_Aint *size, int *disp_unit, void *baseptr, int *ierror)
{
	mpi_win_shared_query_(win, rank, size, disp_unit, baseptr, ierror);
}
```

## mpijson.py

Generate a JSON view of the bindings

Used for Visual Studio CODE plugin (https://github.com/gweodoo/mpi-snippets-vscode)

```
python mpijson.py
```

```json
    {"MPI_Win_shared_query": [
        [
            "MPI_Win",
            "win"
        ],
        [
            "int",
            "rank"
        ],
        [
            "MPI_Aint*",
            "size"
        ],
        [
            "int*",
            "disp_unit"
        ],
        [
            "void*",
            "baseptr"
        ]
    ]}
```
