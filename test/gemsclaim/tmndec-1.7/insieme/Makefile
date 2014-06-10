INSIEME_LIBS_DIR=/insieme-libs
INSIEME_DIR=/home/alessi/Insieme

fix:
	sed -i s/_XEvent/union\ _XEvent/g insieme.c
	sed -i s/struct\ _anonRecord_usr_include_X11_Xlib_h5019/struct\ _XDisplay/g insieme.c
	sed -i s/struct\ _anonRecord_usr_include_X11_Xlib_h9329/XErrorEvent/g insieme.c

gcc:
	#gcc insieme.c --std=c99 -lm -lXext -lX11 -o insieme -g3 -D_XOPEN_SOURCE
	gcc -g3 insieme.c -I${INSIEME_LIBS_DIR}/papi-latest/include/ -I${INSIEME_DIR}/insieme_master/code/meta_information/include -I${INSIEME_DIR}/insieme_master/code/runtime/include -std=c99 -o insieme -lXext -lX11 -lpthread -lm -lrt -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -DIRT_ENABLE_REGION_INSTRUMENTATION -DIRT_ENABLE_INSTRUMENTATION -DIRT_ENABLE_OMPP_OPTIMIZER -DIRT_INSTRUMENTATION_OUTPUT_TO_STDERR -DIRT_USE_PAPI -lpapi -L${INSIEME_LIBS_DIR}/papi-latest/lib  #-DIRT_VERBOSE

run: 
	./insieme -o4 ../test.h263

insieme:
	~/Insieme/build_master/code/driver/main ../*.c -b seq -o insieme.c -DDISPLAY  -DSH_MEM -DUSE_TIME
