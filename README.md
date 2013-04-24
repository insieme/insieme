# Insieme: Compiler and Runtime Infrastructure
The Insieme compiler is a source-to-source compiler for C/C++ that supports portable parallel abstractions (OpenMP, MPI and OpenCL) for heterogeneous multi-core architectures. More deatails are available [here](http://insieme-compiler.org/mission.html).

## Installation 
Insieme is written in C++11 and relies on several third-party libraries, an updated list is available [here](http://insieme-compiler.org/license.html). 

### Prepare the Environment

Name 		| version 
------------|--------|
[G++](http://gcc.gnu.org/gcc-4.6/)	                                | = 4.6.x  |
[LLVM/Clang](http://llvm.org/) 	                                    | = 3.2 ([patch](https://github.com/insieme/insieme/blob/master/scripts/patches/insieme-clang-3.2.patch)) |
[Google Test](https://code.google.com/p/googletest/)                | >= 1.6  |
[Xerces-C++](http://xerces.apache.org/xerces-c/)                    | >= 3.1.1 |
[Boost](http://www.boost.org/users/history/version_1_50_0.html)		  | = 1.50 |
[ISL](http://garage.kotnet.org/~skimo/isl/)			                    | = 0.10 |
[CLooG](http://www.cloog.org/)		                                  | = 0.17 |
[Barvinok](http://garage.kotnet.org/~skimo/barvinok/)               | = 0.35 |
[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)	  	                  | >= 2.4.2 |
[LuaJIT](http://luajit.org/)                                  		  | >= 2.0.0 |
[Shark](http://image.diku.dk/shark/sphinx_pages/build/html/index.html)	| >= 2.3.4 |
[Kompex](http://sqlitewrapper.kompex-online.com/)             	  	| >= 1.7.9 |
[cmake](http://www.cmake.org/)                                      | >= 2.8.x |

You can either install those packages manually (or via a package manager) or the provided utility which takes care of build all dependencies from scratch and apply patches. 

```
cd scripts
make PREFIX=/where/to/install
```

All libraries will be installed in $PREFIX and a symbolic link is created with postfix '-latest' pointing to the newest installed version of a library (which simplifies the following cmake configuration steps)






