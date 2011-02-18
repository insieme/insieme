SIMPLE_BACKEND_RUNTIME_HOME=/insieme-dev/herbert/build_all/code/simple_backend/

TIME_CMD=/usr/bin/time
TIME_FORMAT="    Total Time: %E\tUSER: %U\tSYS: %S\tContextSwitches: %c"

if [ $# -ge 1 ] ; then
	TESTCASES=$1
else
	TESTCASES=`more ../test.cfg`
fi

for CASE in $TESTCASES ; do

        echo
        echo "Running test $CASE ..."
	echo "  GCC Version:"
        $TIME_CMD -f "$TIME_FORMAT" ./$CASE.ref  > /dev/null
	mv gmon.out ${CASE}_profile_gcc.out
	gprof -l -p $CASE.ref ${CASE}_profile_gcc.out > ${CASE}_profile_gcc.txt

	echo "  Insieme Version:"
        LD_LIBRARY_PATH=$SIMPLE_BACKEND_RUNTIME_HOME $TIME_CMD -f "$TIME_FORMAT" ./$CASE.test > /dev/null
	mv gmon.out ${CASE}_profile_insieme.out
	gprof -l -p $CASE.test ${CASE}_profile_insieme.out > ${CASE}_profile_insieme.txt
done

