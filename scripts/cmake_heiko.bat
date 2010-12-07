@echo off


REM ---  Go to the directory this script lies in
%~d0
cd %~dp0


REM ---  Go to code directory
cd ../code


REM --- Remove and recreate build directory
rm -rf build
mkdir build
cd build


REM --- Call cmake
@echo on
cmake .. -G "Visual Studio 10" -DGTEST_ROOT=%GTEST_ROOT% -Dpthread_LIB=%PTHREAD_ROOT%\lib\pthreadVC2.lib -DLLVM_HOME="C:/Program Files (x86)/LLVM" -DGLOG_HOME=%GLOG_ROOT%\ -DXERCES_INCLUDE_DIR=%XERCES_ROOT_DIR%\include -DXERCES_C_LIBRARY=%XERCES_ROOT_DIR%\lib\xerces-c_static_3.lib -DLINKING_TYPE=STATIC
@echo off

rem DEBUG: cmake .. -G "Visual Studio 10" -DGTEST_ROOT=%GTEST_ROOT% -Dpthread_LIB=%PTHREAD_ROOT%\lib\pthreadVC2.lib -DLLVM_HOME=%LLVM_2_8% -DGLOG_HOME=%GLOG_ROOT%\ -DXERCES_INCLUDE_DIR=%XERCES_ROOT_DIR%\include -DXERCES_C_LIBRARY=%XERCES_ROOT_DIR%\lib\xerces-c_static_3d.lib -DLINKING_TYPE=STATIC


REM --- Wait for user
pause
