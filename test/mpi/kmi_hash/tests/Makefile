#   ---------------------------
#   Compiler Flags
#   ---------------------------

#MPICC = mpixlc_r 
#MPICXX = mpicxx
#MPICC = mpicc 
#MPICXX = mpicxx
#MPICC = /software-local/insieme-dev/philipp/buildall_insieme_base_debug/code/driver/insiemecc -I/software-local/insieme-libs/openmpi-latest/include
MPICC = /software-local/insieme-dev/philipp/buildall_insieme_base_debug/code/driver/mpimodelcc -I/software-local/insieme-libs/openmpi-latest/include
MPICXX = $(MPICC)

KMI_LIB = -L../src -lkmi

# HPM_LIB = /soft/perftools/hpctw/lib/libmpihpm.a /bgsys/drivers/ppcfloor/bgpm/lib/libbgpm.a 

MPI_CFLAGS = -I../src -O3 -pg -DKMI_CONFIG_BINARY_STRING -DKMI_CONFIG_SORTING -DKMI_CONFIG_INDEXED_ARRAY -DKMI_CONFIG_SAMPLING_ITERATION -DKMI_CONFIG_INDEXED_ARRAY_HASH -DKMI_PROFILING_ALLTOAL

all: BENCH_QUERY 

KMI_C_OBJ2 = ../src/binary.o  ../src/db.o  ../src/debug.o  ../src/hash_table.o  ../src/init.o  ../src/iquery.o  ../src/malloc.o  ../src/partition.o  ../src/query.o  ../src/sorted_array.o  ../src/string.o  ../src/table.o 
KMI_C_OBJ = bench_query.o $(KMI_C_OBJ2)

$(KMI_C_OBJ): %.o: %.c
	$(MPICC) $(MPI_CFLAGS) -c $< -o $@

BENCH_QUERY: $(KMI_C_OBJ)
	$(MPICC) -o BENCH_QUERY $(MPI_CFLAGS) $(KMI_C_OBJ) $(KMI_LIB) $(HPM_LIB) 

clean:
	rm -f *.a *.o a.out core* BENCH_QUERY
