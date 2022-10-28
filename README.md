This code is a collection of matrix derivative tests for forward and reverse mode
algorithmic differentiation.
        
# Getting started [![CMake](https://github.com/octoflar/mad/actions/workflows/cmake.yml/badge.svg)](https://github.com/octoflar/mad/actions/workflows/cmake.yml)
 
Building this software requires [CMake](https://cmake.org) and a compiler that implements
the Fortran 2008 standard.

To build and run the matrix tests `cd` into the project root directory and type:

    mkdir cmake-build
    cd cmake-build
    cmake -DCMAKE_BUILD_TYPE=(Release|Debug) ..
    make all test

To use a specific Fortran compiler set the `FC` and `CC` environment variables, like

    export FC=gfortran
    export CC=gcc

*before* you execute the `cmake ...` command.

# Further reading

Giles, M. (2008) "An extended collection of matrix derivative results for forward and reverse mode algorithmic differentiation."  
<https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf>.

Giles, M. (2008) "Collected matrix derivative results for forward and reverse mode algorithmic differentiation."  
<https://doi.org/10.1007/978-3-540-68942-3_4>
