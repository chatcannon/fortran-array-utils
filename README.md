*fortran-array-utils*
===================

Syntactic sugar for resizing Fortran allocatable arrays (and related actions such as appending) 

The target use case for this module is reading in data from a file or files, where it is not known in advance how big the file is.
It is assumed that reading the data is not time-critical, but that the data once read may be used in time-critical inner loops of the application.
For this reason the code does more copying and reallocating than would be strictly necessary, so that normal allocatable arrays of the correct size
are always used.

N.B. The code generation requires python 2 and Cheetah. The 'generated' branch contains a pre-generated copy of array-utils.F90

    resize(array, {dimensions}, [fill])
will resize `array` to `dimensions`, copying as much of the array as will fit inside the new dimensions and optionally filling any remaining space with `fill`

    expand(array, {dimensions}, [fill])
is like `resize` but will only ever make the array bigger, not smaller

    insert(array, {dimensions}, item, [fill])
inserts `item` at the position given by `dimensions`, expanding the array if necessary

    insert_row(array, {dimensions}, item, [fill])
like `insert`, but inserts a whole vector at a time (the dimension of the vector is the first dimension in the array, and `dimensions` gives the remaining dimensions)

