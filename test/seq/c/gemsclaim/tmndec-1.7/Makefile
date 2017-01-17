# Makefile for tmndec <kol@nta.no>

# This is free software. See the file COPYING for copying conditions.

# uncomment the following two lines if you want to include X11 support
USE_DISP = -DDISPLAY 
LIBS = -lX11

# uncomment the following two lines if you want to use shared memory
# (faster display if server and client run on the same machine)
USE_SHMEM = -DSH_MEM
LIBS = -lXext -lX11

# uncomment the following line to activate calculation of decoding speed
# (frames per second) and frame rate control (-fn option)
USE_TIME = -DUSE_TIME

# if your X11 include files / libraries are in a non standard location:
# set INCLUDEDIR to -I followed by the appropriate include file path and
# set LIBRARYDIR to -L followed by the appropriate library path and

# Necessary for OpenWindows
#INCLUDEDIR = -I/usr/openwin/include
#LIBRARYDIR = -L/usr/openwin/lib -Wl,-R,/usr/openwin/lib

# Necessary on some Linux configs
LIBRARYDIR=-L/usr/X11R6/lib



#
# GNU gcc
#

CC = gcc
CFLAGS = -g -O $(DEFS) $(USE_DISP) $(USE_SHMEM) $(USE_TIME) $(INCLUDEDIR)


OBJ = tmndec.o getpic.o getvlc.o gethdr.o getblk.o getbits.o store.o recon.o idct.o idctref.o display.o sac.o dither.o yuv2rgb.o yuvrgb24.o win.o

all: tmndec

clean:
	rm -f *.o *% core tmndec


tmndec: $(OBJ)
	$(CC) $(CFLAGS) $(LIBRARYDIR) -o tmndec $(OBJ) -lm $(LIBS)

tar: 
	tar cvf tmndec.tar *.c *.h *.mak Makefile README COPYING CHANGES TODO
	gzip tmndec.tar
	mv -f tmndec.tar.gz tmndec.tgz

display.o : display.c config.h global.h tmndec.h 
getbits.o : getbits.c config.h global.h tmndec.h 
getblk.o : getblk.c config.h global.h tmndec.h getvlc.h
gethdr.o : gethdr.c config.h global.h tmndec.h 
getpic.o : getpic.c config.h global.h tmndec.h getvlc.h indices.h sactbls.h 
getvlc.o : getvlc.c config.h global.h tmndec.h getvlc.h 
idct.o : idct.c config.h 
idctref.o : idctref.c config.h 
tmndec.o : tmndec.c config.h global.h tmndec.h 
recon.o : recon.c config.h global.h tmndec.h 
store.o : store.c config.h global.h tmndec.h 
sac.o : sac.c tmndec.h config.h global.h
dither.o: dither.c config.h global.h
yuv2rgb.o: yuv2rgb.c config.h global.h
yuvrgb24.o: yuvrgb24.c config.h global.h
win.o: win.c win.h config.h tmndec.h global.h


