# Insieme: Compiler and Runtime Infrastructure
The Insieme compiler is a source-to-source compiler for C/C++ that supports portable parallel abstractions (OpenMP, MPI and OpenCL) for heterogeneous multi-core architectures. More deatails are available [here](http://insieme-compiler.org/mission.html).

## Installation 
Insieme is written in C++11 and relies on several third-party libraries, an updated list is available [here](http://insieme-compiler.org/license.html). 

### List of Required Libraries and Software
Name 		| Version | What's for |
--------|---------|------------|
[G++](http://gcc.gnu.org/gcc-4.6/)	                                | = 4.6.x  | Compiler |
[CMake](http://www.cmake.org/)                                      | >= 2.8.x | Build System |
[Google Test](https://code.google.com/p/googletest/)                | >= 1.6  | Unit testing |
[Boost](http://www.boost.org/users/history/version_1_50_0.html)  	  | = 1.50 | Utilities, regex, filesystem, program options|
[LLVM/Clang](http://llvm.org/) 	                                    | = 3.2 ([patch](https://github.com/insieme/insieme/blob/master/scripts/patches/insieme-clang-3.2.patch)) | C/C++ frontend | 
[Xerces-C++](http://xerces.apache.org/xerces-c/)                    | >= 3.1.1 | XML dump of the INSPIRE |
[ISL](http://garage.kotnet.org/~skimo/isl/)			                    | = 0.10 | Polyhedral model representation & analysis |
[CLooG](http://www.cloog.org/)		                                  | = 0.17 | Polyhedral model code generation |
[Barvinok](http://garage.kotnet.org/~skimo/barvinok/)               | = 0.35 | Polyhedral model cardinality |
[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)	  	                  | >= 2.4.2 | Manipulation of decision diagrams |
[LuaJIT](http://luajit.org/)                                  		  | >= 2.0.0 | Scripting |
[Shark](http://image.diku.dk/shark/sphinx_pages/build/html/index.html)	| >= 2.3.4 | Machine learning |
[Kompex](http://sqlitewrapper.kompex-online.com/)             	  	| >= 1.7.9 | DBMS |
[Ruby](http://www.ruby-lang.org/en/)                                | >= 2.8 | Scripting |

### Prepare the Environment
You can either install those packages manually (or via a package manager) or use the provided utility which takes care of building all dependencies from scratch and apply patches. 
```
cd scripts
make PREFIX=/where/to/install 
```
All libraries will be installed in $PREFIX and a symbolic link is created with postfix '-latest' pointing to the newest installed version of a library (which simplifies the following cmake configuration steps). By default PREFIX=~/lib; additionally the script performs a parallel build (make -j$SLOTS) using all availabe cores in the host architecture. You can change this behaviour by passing to the make a different value for SLOTS.

### Build Insieme
Insieme's build system is based on CMake. We suppose to have the all dependencies installed in the same directory: $HOME/libs
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
- -DDOC=ON|OFF -- Enable/disable generation of documentation 
- -DUSE_XML=ON|OFF -- Enable/disable XML support 
- -DMAKE_BUILD_TYPE=Debug|Release -- Set the build to be in Debug or Release mode 
- -DUSE_OPENCL=ON|OFF -- Enabe/disable OpenCL support (in both frontend/backend/runtime)
(note: this variables must be passed as parameters of the cmake command, not set as environment variables)

If successful, CMake produces the set of Makefiles to build the Insieme project. 

```
make -j2 # Builds using two threads
```

Or you can alternatively mode into a subfolder and only buld that particular module of the compiler
```
cd core
make
```

Unit tests can be executed with:
```
make test
```

If everyting is successful... **congratulation!**
**You may start enjoying Insieme now!**

## Using Insieme 



