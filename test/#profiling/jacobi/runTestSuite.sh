
SIMPLE_BACKEND_RUNTIME_HOME=../../../build_all/code/simple_backend/

TIME_CMD=/usr/bin/time
TIME_FORMAT="Total Time: %E\tUSER: %U\tSYS: %S\tContextSwitches: %c"

N=2000

echo
echo "Running reference version ..."
for i in 1 2 4 8 ; do
	export OMP_NUM_THREADS=$i
	taskset -c 1-$i $TIME_CMD -f "$TIME_FORMAT" ./jacobi_gcc $N > /dev/null
done

echo
echo "Running insieme version ..."
for i in 1 2 4 8 ; do
	export ISBR_NUMTHREADS=$i 
	LD_LIBRARY_PATH=$SIMPLE_BACKEND_RUNTIME_HOME taskset -c 1-$i $TIME_CMD -f "$TIME_FORMAT" ./jacobi_insieme $N > /dev/null
done
