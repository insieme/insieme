INSIEME_LIBS_DIR=/insieme-libs
INSIEME_DIR=/home/alessi/Insieme

all: insieme fix gcc

fix:
	sed -i s/struct\ _anonRecord_usr_include_X11_Xlib_h5019/struct\ _XDisplay/g insieme.c
	sed -i s/struct\ _anonRecord_usr_include_X11_Xlib_h9329/XErrorEvent/g insieme.c

gcc:
	#gcc insieme.c --std=c99 -lm -lXext -lX11 -o insieme -g3 -D_XOPEN_SOURCE
	gcc -O3 insieme.c -I${INSIEME_LIBS_DIR}/papi-latest/include/ -I${INSIEME_DIR}/insieme_master/code/meta_information/include -I${INSIEME_DIR}/insieme_master/code/runtime/include -std=c99 -o insieme -lXext -lX11 -lpthread -lm -lrt -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -DIRT_ENABLE_REGION_INSTRUMENTATION -DIRT_ENABLE_INSTRUMENTATION -DIRT_INSTRUMENTATION_OUTPUT_TO_STDERR -DIRT_USE_PAPI -lpapi -L${INSIEME_LIBS_DIR}/papi-latest/lib  #-DIRT_VERBOSE -DIRT_ENABLE_OMPP_OPTIMIZER 

run: 
	IRT_NUM_WORKERS=4 IRT_INST_REGION_INSTRUMENTATION_TYPES="cpu_energy,cpu_time,wall_time"  IRT_INST_WORKER_EVENT_TYPES="WI,WG" IRT_INST_WORKER_EVENT_LOGGING="enabled" IRT_INST_BINARY_OUTPUT="enabled" IRT_INST_REGION_INSTRUMENTATION="enabled" IRT_AFFINITY_POLICY="IRT_AFFINITY_FIXED,0,1,2,3" ./insieme -o4 ../burn.h263

insieme:
	~/Insieme/build_master/code/driver/main ../*.c -b run --omp-sema -o insieme.c -DDISPLAY  -DSH_MEM -DUSE_TIME

gdb:
	IRT_NUM_WORKERS=4 IRT_INST_REGION_INSTRUMENTATION_TYPES="cpu_energy,cpu_time,wall_time"  IRT_INST_WORKER_EVENT_TYPES="WI,WG" IRT_INST_WORKER_EVENT_LOGGING="enabled" IRT_INST_BINARY_OUTPUT="enabled" IRT_INST_REGION_INSTRUMENTATION="enabled" IRT_AFFINITY_POLICY="IRT_AFFINITY_FIXED,0,1,2,3" gdb insieme

clean:
	rm -f insieme* worker*

linux:
	gcc ../*.c -lXext -lX11 -lm -fopenmp -DDISPLAY  -DSH_MEM -DUSE_TIME -std=c99 -D_SVID_SOURCE
