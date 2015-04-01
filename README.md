# Insieme: Compiler and Runtime Infrastructure
The Insieme compiler is a source-to-source compiler for C/C++ that supports portable parallel abstractions (OpenMP, MPI and OpenCL) for heterogeneous multi-core architectures. More details are available [here](http://insieme-compiler.org/mission.html).

## Directory Structure
Insieme contains 4 main sub-directories:

* insieme
  * /code -- Insieme compiler implementation
  * /docs -- Insieme developer documentation
  * /scripts -- Insieme environment installation scripts
  * /test -- Insieme integration tests

## Installation 
Insieme is written in C++11 and relies on several third-party libraries: 

### List of Required Libraries and Software
Name 		| Version | What's for |
--------|---------|------------|
[G++](http://gcc.gnu.org/gcc-4.7/)	                                | >= 4.7  | Compiler |
[CMake](http://www.cmake.org/)                                      | >= 2.8.x | Build System |
[Google Test](https://code.google.com/p/googletest/)                | >= 1.6  | Unit testing |
[Boost](http://www.boost.org/users/history/version_1_50_0.html)  	  | = 1.50 | Utilities, regex, filesystem, program options|
[LLVM/Clang](http://llvm.org/) 	                                    | = 3.4 ([patch](https://github.com/insieme/insieme/blob/master/scripts/patches/insieme-clang-3.4.patch)) | C/C++ frontend | 
[ISL](http://garage.kotnet.org/~skimo/isl/)			                    | = 0.10 | Polyhedral model representation & analysis |
[CLooG](http://www.cloog.org/)		                                  | = 0.17 | Polyhedral model code generation |
[Barvinok](http://garage.kotnet.org/~skimo/barvinok/)               | = 0.35 | Polyhedral model cardinality |
[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)	  	                  | >= 2.4.2 | Manipulation of decision diagrams |
[LuaJIT](http://luajit.org/)                                  		  | >= 2.0.0 | Scripting |
[Shark](http://image.diku.dk/shark/sphinx_pages/build/html/index.html)	| >= 2.3.4 | Machine learning |
[Kompex](http://sqlitewrapper.kompex-online.com/)             	  	| >= 1.7.9 | DBMS |
[Ruby](http://www.ruby-lang.org/en/)                                | >= 2.8 | Scripting |

### List of Optional Libraries and Software
Name 		| Version | What's for |
--------|---------|------------|
[PAPI](http://icl.cs.utk.edu/papi/)	                                | >= 5.4.0  | Runtime system, for hardware information and event counters |

### Preparing the Environment
You can either install those packages manually (or via a package manager) or use the provided utility which takes care of building all dependencies from scratch and applies patches. 
```
cd scripts
make PREFIX=/where/to/install 
```
All libraries will be installed in ``$PREFIX`` and a symbolic link is created with postfix '-latest' pointing to the newest installed version of a library (which simplifies the following CMake configuration steps). By default ``PREFIX=~/lib``; additionally the script performs a parallel build (``make -j$SLOTS``) using all availabe cores in the host architecture. You can change this behaviour by passing a different value for ``SLOTS`` to ``make``.

### Building Insieme
Insieme's build system is based on CMake. We assume that all dependencies installed in the same directory: ``$HOME/libs``

```
mkdir build
cd build

# SET ENVIRONMENT 
export PREFIX=$HOME/libs
export LD_LIBRARY_PATH=$PREFIX/gcc-latest/lib64:$PREFIX/mpfr-latest/lib:$PREFIX/mpc-latest/lib:\\
  $PREFIX/gmp-latest/lib:$PREFIX/cloog-gcc-latest/lib:$PREFIX/ppl-latest/lib:$PREFIX/papi-latest/lib:$LD_LIBRARY_PATH
export PATH=$PREFIX/ruby-latest/bin/:$PREFIX/cmake-latest/bin:$PATH

# CREATE PROJECT MAKEFILE
CXX=$PREFIX/gcc-latest/bin/g++ INSIEME_LIBS_HOME=$PREFIX $PREFIX/cmake-latest/bin/cmake ..
```

Several other options can be provided to enable/disable some of the compiler features:
- ``-DDOCS=ON|OFF`` -- Enable/disable generation of documentation
- ``-DCMAKE_BUILD_TYPE=Debug|Release`` -- Set the build to be in Debug or Release mode 
- ``-DUSE_OPENCL=ON|OFF`` -- Enabe/disable OpenCL support (in both frontend/backend/runtime)
- ``-DUSE_ENERGY=ON|OFF`` -- Enable/disable energy measurement support
- ``-DCONDUCT_MEMORY_CHECKS=ON|OFF`` -- Enable/disable valgrind memory checks for most unit tests
(note: these variables must be passed as parameters to the ``cmake`` command, not set as environment variables)

If successful, CMake produces the set of Makefiles required to build the Insieme project. 

```
make -j2 # Builds using two parallel jobs
```

Or you can alternatively change into a subfolder and only build that particular module of the compiler
```
cd core
make
```

### Running Unit Tests

Unit tests can be executed with:
```
make test ARGS=-j2 # Runs all unit tests using two parallel jobs
```

or directly via CTest:
```
ctest -j2 # Runs all unit tests using two parallel jobs
```


If everything was successful... 
**congratulations!**
**You can start enjoying Insieme now!**


Please, understand that the install command is not implemented since this is an on-going development framework. 
Instead of polluting your local setup, we prefer to use Insieme from the build directory. 
Install scripts will be provided in future releases.

### Running Integration Tests

Integration tests can be executed using the custom runner compiled in the build directory:
```
./code/driver/integration_tests
```
Integration tests can be executed in parallel (``-w SLOTS``), and multiple times (``-r N``). For a full list of options use the ``-h`` argument or refer to the Insieme developer documentation. The mock run (``-m``) will give you an idea of the actual commands being invoked.

### Compiling Application Codes

The main executable provided by the Insieme framework is called ``insiemecc``. It can be used to replace e.g. occurrences of another compiler such as ``gcc`` in makefiles. It supports both source-to-source-only compilation, as well as full compilation by calling a backend compiler. For further information on its features and options, please refer to:
```
./code/driver/insiemecc --help
```
