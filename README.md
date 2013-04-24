# Insieme: Compiler and Runtime Infrastructure
The Insieme compiler is a source-to-source compiler for C/C++ that supports portable parallel abstractions (OpenMP, MPI and OpenCL) for heterogeneous multi-core architectures. More deatails are available [here](http://insieme-compiler.org/mission.html).

## Installation 
Insieme is written in C++11 and relies on several third-party libraries, an updated list is available [here](http://insieme-compiler.org/license.html). 

### Required software and libraries 

Name 		| version 
------------|--------|
G++ 		| = 4.6.3  |
LLVM/Clang 	| = 3.2 (patch) |
Google Test | >= 1.3  |
Xerces      | >= 3.1.1 |
Boost		| = 1.50 |
ISL			| = 0.10 |
ClooG		| = 0.17 |
Barvinok	| = 0.35 |
CUDD		| >= 2.4.2 |
LuaJIT		| >= 2.0.0 |
Shark		| >= 2.3.4 |
Kompex		| >= 1.7.9 |

