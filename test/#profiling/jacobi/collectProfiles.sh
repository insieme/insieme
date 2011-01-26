
SIMPLE_BACKEND_RUNTIME_HOME=../../../build_all/code/simple_backend/

TIME_CMD=/usr/bin/time
TIME_FORMAT="Total Time: %E\tUSER: %U\tSYS: %S\tContextSwitches: %c"

N=2000

NUM_THREADS=1

export OMP_NUM_THREADS=$NUM_THREADS
export ISBR_NUMTHREADS=$NUM_THREADS

. buildCases.sh

for v in insieme gcc  ; do

	echo
	echo "Running $v version ..."
	LD_LIBRARY_PATH=$SIMPLE_BACKEND_RUNTIME_HOME taskset -c 1 $TIME_CMD -f "$TIME_FORMAT" ./jacobi_$v $N > /dev/null
	mv gmon.out profile_$v.out
	gprof -l jacobi_$v profile_$v.out > profile_$v.txt

done

