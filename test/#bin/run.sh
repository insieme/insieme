SIMPLE_BACKEND_RUNTIME_HOME=../../build_all/code/simple_backend/

TIME_CMD=/usr/bin/time
TIME_FORMAT="    Total Time: %E\tUSER: %U\tSYS: %S\tContextSwitches: %c"

NUM_TEST_RUNS=`seq 3`

if [ $# -ge 1 ] ; then
	TESTCASES=$1
else
	TESTCASES=`more ../test.cfg`
fi

for CASE in $TESTCASES ; do

        echo
        echo "Running test $CASE ..."
	echo "  GCC Version:"
	for i in $NUM_TEST_RUNS ; do
        	$TIME_CMD -f "$TIME_FORMAT" ./$CASE.ref  > /dev/null
	done

	echo "  Insieme Version:"
	for i in $NUM_TEST_RUNS ; do
        	LD_LIBRARY_PATH=$SIMPLE_BACKEND_RUNTIME_HOME $TIME_CMD -f "$TIME_FORMAT" ./$CASE.test > /dev/null
	done
done

