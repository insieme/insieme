#
# The default versions for the different libraries used by insieme
# can be overwritten by the user with an env-variable
# we rely on capitalized variables: LIBNAME_VERSION

#LLVM/CLANG
set(LLVM_VERSION 3.4)

#XERCES  - used by: xml
set(XERCES_VERSION 3.1.1)

#PAPI - used by: driver, runtime
set(PAPI_VERSION 5.4.0)

#ISL - used by: analysis
set(ISL_VERSION 0.10)

#CLOOG - used by: analysis
set(CLOOG_VERSION 0.17.0)

#BARVINOK - used by: analysis
set(BARVINOK_VERSION 0.35)

#MPFR - used by: analysis
set(MPFR_VERSION 3.1.1)

#GMP - used by: analysis
set(GMP_VERSION 6.0.0)

#CUDD - used by: core
set(CUDD_VERSION 2.4.2)

#LUAJIT - used by: utils
set(LUAJIT_VERSION 2.0.3)

#SHARK - used by: machine_learning
set(SHARK_VERSION 2.3.4)

#KOMPEX - used by: machine_learning
set(KOMPEX_VERSION 1.7.9)

#GTEST - used for unit tests
set(GTEST_VERSION 1.7.0)

#BOOST - used everywhere except for runtime...
set(BOOST_VERSION 1.50.0)
