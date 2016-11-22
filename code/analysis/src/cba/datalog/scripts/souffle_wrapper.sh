#!/bin/bash

ldlp="$1"
shift
souffle="$1"
shift
arguments=$@

echo "--------------------"
echo "HAVE LDLP = $ldlp"
echo "HAVE SOUFFLE = $souffle"
echo "HAVE ARGUMENTS = $arguments"
echo "~~~~~~~~~~~~~~~~~~~~"


export LD_LIBRARY_PATH=$ldlp:$LD_LIBRARY_PATH
$souffle $arguments
exit $?
