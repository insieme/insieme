CC = "gcc"
CXX = "g++"
CFLAGS = -Wall -DUNIX -fPIC -DDEBUG -g -std=c99
SRCDIR = ./src/
OUTDIR = ./bin/
INCLUDEDIR = ./include/
INCLUDE = -I/home/klaus/insieme/code/frontend/test/inputs -I$(INCLUDEDIR) -L/home/klaus/AMD-APP-SDK-v2.4-lnx64/lib/x86_64 -lOpenCL 

all: main 

hashFct: $(SRCDIR)hash_functions.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)hash_functions.c -I./include -c -o $(OUTDIR)hash_functions.o

hashTable: hashFct $(SRCDIR)hash_table.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)hash_table.c -I./include -c -o $(OUTDIR)hash_table.o

element: $(SRCDIR)Element.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)Element.c $(INCLUDE) $(LIBS) -c -o $(OUTDIR)element.o

material: $(SRCDIR)Material.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)Material.c $(INCLUDE) $(LIBS) -c -o $(OUTDIR)material.o

cLElemQMHS4: element material $(SRCDIR)CLElemQMHS4.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)CLElemQMHS4.c $(INCLUDE) $(LIBS) -c -o $(OUTDIR)cLElemQMHS4.o
	
readData: $(SRCDIR)readData.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)readData.c -I./include -c -o $(OUTDIR)readData.o

saveData: $(SRCDIR)saveData.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)saveData.c -I./include -c -o $(OUTDIR)saveData.o

loadCheckSave: readData saveData $(SRCDIR)loadCheckSave.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)loadCheckSave.c $(OUTDIR)readData.o $(OUTDIR)saveData.o $(INCLUDE) -o $(OUTDIR)loadCheckSave.o

main: cLElemQMHS4 saveData readData hashTable hashFct $(SRCDIR)main.c
	$(CC) $(CFLAGS) -fPIC $(SRCDIR)main.c $(OUTDIR)cLElemQMHS4.o $(OUTDIR)saveData.o $(OUTDIR)readData.o $(OUTDIR)hash_table.o $(OUTDIR)hash_functions.o $(OUTDIR)material.o $(OUTDIR)element.o $(INCLUDE) $(LIBS) -o $(OUTDIR)run

utils: $(SRCDIR)utils.c
	$(CC) $(CFLAGS) -shared $(SRCDIR)utils.c $(INCLUDE) -c -o $(OUTDIR)utils.o

crap: utils loadCheckSave hashTable hashFct $(SRCDIR)all.c
	$(CC) $(CFLAGS) -fPIC $(SRCDIR)all.c $(OUTDIR)utils.o $(OUTDIR)loadCheckSave.o $(OUTDIR)hash_table.o $(OUTDIR)hash_functions.o $(OUTDIR)material.o $(OUTDIR)element.o -I./include $(INCLUDE) $(LIBS) -o $(OUTDIR)crap

clean:
	rm ./bin/*.o ./bin/run
