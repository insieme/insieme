INSIEME_BUILD_DIR=/home/herbert/insieme/build_eclipse
INSIEME_SRC_DIR=/home/herbert/insieme/code/driver

CFLAGS=--std=c99

is:	is.insieme.o $(INSIEME_SRC_DIR)/test/nas/common/wtime.o $(INSIEME_SRC_DIR)/test/nas/common/c_timers.c

is.insieme.o: is.insieme.c

is.insieme.c: $(INSIEME_SRC_DIR)//test/nas/is/is.c
	$(INSIEME_BUILD_DIR)/code/driver/main -I$(INSIEME_SRC_DIR)/test/nas/is -I$(INSIEME_SRC_DIR)/test/nas/common \
			 $(INSIEME_SRC_DIR)/test/nas/is/is.c \
			 $(INSIEME_SRC_DIR)/test/nas/common/c_timers.c \
			 $(INSIEME_SRC_DIR)/test/nas/common/c_print_results.c -P --dump-ir=is.inspire --show-ir=is.dot -o is.insieme.c
	
clean:
	rm -f is.insieme.c 
