INSIEME_BUILD_DIR=/home/motonacciu/workspace/insieme2_build/code/driver/

is:	is.insieme.o ../common/c_print_results.o ../common/c_timers.o ../common/wtime.o ../common/wtime_sgi64.c

is.insieme.o: is.insieme.c

is.insieme.c: is.c
	$(INSIEME_BUILD_DIR)/main -I. -I../common is.c -o is.insieme.c
	
clean:
	rm -f is.insieme.c 