/*
Copyright 2007, 2008 Daniel Zerbino (zerbino@ebi.ac.uk)

    This file is part of Velvet.

    Velvet is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Velvet is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Velvet; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#include <uce-dirent.h>
#define Arc v_Arc
#else
#include <dirent.h>
#endif

#include "run.h"

static void printUsage()
{
	puts("Usage:");
	puts("./velveth directory hash_length {[-file_format][-read_type][-separate|-interleaved] filename1 [filename2 ...]} {...} [options]");
	puts("");
	puts("\tdirectory\t: directory name for output files");
	printf("\thash_length\t: EITHER an odd integer (if even, it will be decremented) <= %i (if above, will be reduced)\n", MAXKMERLENGTH);
	printf("\t\t\t: OR: m,M,s where m and M are odd integers (if not, they will be decremented) with m < M <= %i (if above, will be reduced)\n", MAXKMERLENGTH);
	puts("\t\t\t\tand s is a step (even number). Velvet will then hash from k=m to k=M with a step of s");
	puts("\tfilename\t: path to sequence file or - for standard input");	
	puts("");
	puts("File format options:");
	puts("\t-fasta\t-fastq\t-raw\t-fasta.gz\t-fastq.gz\t-raw.gz\t-sam\t-bam\t-fmtAuto");
        puts("\t(Note: -fmtAuto will detect fasta or fastq, and will try the following programs for decompression : gunzip, pbunzip2, bunzip2");
	puts("");
        puts("File layout options for paired reads (only for fasta and fastq formats):");
        puts("\t-interleaved\t: File contains paired reads interleaved in the one file (default)");
        puts("\t-separate\t: Read 2 separate files for paired reads");
        puts("");
	puts("Read type options:");
	puts("\t-short\t-shortPaired");
#if CATEGORIES <= 5
	Category cat; 
	for (cat = 2; cat <= CATEGORIES; cat++)
	    printf("\t-short%i\t-shortPaired%i\n", cat, cat);
#else
	puts("\t...");
	printf("\t-short%i\t-shortPaired%i\n", CATEGORIES - 1, CATEGORIES - 1);
	printf("\t-short%i\t-shortPaired%i\n", CATEGORIES, CATEGORIES);
#endif
	puts("\t-long\t-longPaired");
	puts("\t-reference");
	puts("");
	puts("Options:");
	puts("\t-strand_specific\t: for strand specific transcriptome sequencing data (default: off)");
	puts("\t-reuse_Sequences\t: reuse Sequences file (or link) already in directory (no need to provide original filenames in this case (default: off)");
	puts("\t-reuse_binary\t: reuse binary sequences file (or link) already in directory (no need to provide original filenames in this case (default: off)");
	puts("\t-noHash\t\t\t: simply prepare Sequences file, do not hash reads or prepare Roadmaps file (default: off)");
	puts("\t-create_binary  \t: create binary CnyUnifiedSeq file (default: off)");
	puts("");
	puts("Synopsis:");
	puts("");
	puts("- Short single end reads:");
	puts("\tvelveth Assem 29 -short -fastq s_1_sequence.txt");
	puts("");
	puts("- Paired-end short reads (remember to interleave paired reads):");
	puts("\tvelveth Assem 31 -shortPaired -fasta interleaved.fna");
	puts("");
	puts("- Paired-end short reads (using separate files for the paired reads)");
	puts("\tvelveth Assem 31 -shortPaired -fasta -separate left.fa right.fa");
	puts("");
	puts("- Two channels and some long reads:");
	puts("\tvelveth Assem 43 -short -fastq unmapped.fna -longPaired -fasta SangerReads.fasta");
	puts("");
	puts("- Three channels:");
	puts("\tvelveth Assem 35 -shortPaired -fasta pe_lib1.fasta -shortPaired2 pe_lib2.fasta -short3 se_lib1.fa");
	puts("");
	puts("Output:");
	puts("\tdirectory/Roadmaps");
	puts("\tdirectory/Sequences");
	puts("\t\t[Both files are picked up by graph, so please leave them there]");
}

int main(int argc, char **argv)
{
	ReadSet *allSequences = NULL;
	SplayTable *splayTable;
	int hashLength, hashLengthStep, hashLengthMax, h;
	char *directory, *filename, *seqFilename, *baseSeqName, *buf;
	char * token;
	boolean double_strand = true;
	boolean noHash = false;
	boolean multiple_kmers = false;
	char buffer[100];
	DIR *dir;

	setProgramName("velveth");

	if (argc < 4) {
		printf("velveth - simple hashing program\n");
		printf("Version %i.%i.%2.2i\n", VERSION_NUMBER,
		       RELEASE_NUMBER, UPDATE_NUMBER);
		printf("\nCopyright 2007, 2008 Daniel Zerbino (zerbino@ebi.ac.uk)\n");
		printf("This is free software; see the source for copying conditions.  There is NO\n");
		printf("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
		printf("Compilation settings:\n");
		printf("CATEGORIES = %i\n", CATEGORIES);
		printf("MAXKMERLENGTH = %i\n", MAXKMERLENGTH);
#ifdef _OPENMP
		puts("OPENMP");
#endif
#ifdef LONGSEQUENCES
		puts("LONGSEQUENCES");
#endif
#ifdef BIGASSEMBLY
		puts("BIGASSEMBLY");
#endif
#ifdef COLOR
		puts("COLOR");
#endif
#ifdef DEBUG
		puts("DEBUG");
#endif
		printf("\n");
		printUsage();
		return 0;
	}

	strcpy(buffer, argv[2]);
	token = strtok(buffer, ",");
	hashLength = atoi(token);
	token = strtok(NULL, ",");
	if (token == NULL) {
		multiple_kmers = false;
		hashLengthMax = hashLength + 1;
	} else {
		multiple_kmers = true;
		hashLengthMax = atoi(token);
	}
	token = strtok(NULL, ",");
	if (token == NULL) {
		hashLengthStep = 2;
	} else {
		hashLengthStep = atoi(token);
	}

	if (hashLength > MAXKMERLENGTH) {
		velvetLog
		    ("Velvet can't handle k-mers as long as %i! We'll stick to %i if you don't mind.\n",
		     hashLength, MAXKMERLENGTH);
		hashLength = MAXKMERLENGTH;
	} 
	if (hashLength <= 0) {
		velvetLog("Invalid hash length: %s\n", argv[2]);
		printUsage();
		return 0;
	} 
	if (hashLength % 2 == 0) {
		velvetLog
		    ("Velvet can't work with even length k-mers, such as %i. We'll use %i instead, if you don't mind.\n",
		     hashLength, hashLength - 1);
		hashLength--;
	} 

	if (multiple_kmers) {
		if (hashLengthMax > MAXKMERLENGTH + 1) {
			velvetLog
			    ("Velvet can't handle k-mers as long as %i! We'll stick to %i if you don't mind.\n",
			     hashLengthMax, MAXKMERLENGTH + 1);
			hashLengthMax = MAXKMERLENGTH + 1;
		} 
		if (hashLengthMax <= hashLength) {
			velvetLog("hashLengthMin < hashLengthMax is required %s", argv[2]);
			printUsage();
			return 0;
		} 

		if (hashLengthStep <= 0) {
			velvetLog("Non-positive hash length! Setting it to 2\n");
			hashLengthStep = 2;
		}
		if (hashLengthStep % 2 == 1) {
			velvetLog
			    ("Velvet can't work with an odd length k-mer step, such as %i. We'll use %i instead, if you don't mind.\n",
			     hashLengthStep, hashLengthStep + 1);
			hashLengthStep++;
		}
	}

	// check if binary sequences should be used
	int argIndex;
	for (argIndex = 3; argIndex < argc; argIndex++)
		if (strcmp(argv[argIndex], "-create_binary") == 0 || strcmp(argv[argIndex], "-reuse_binary") == 0)
			setCreateBinary(true);

	for (h = hashLength; h < hashLengthMax; h += hashLengthStep) {

		resetWordFilter(h);

		buf = mallocOrExit(2 * strlen(argv[1]) + 500, char);

		if ( multiple_kmers ) {
			sprintf(buf,"%s_%d",argv[1],h);
			directory = mallocOrExit(strlen(buf) + 100, char);
			strcpy(directory,buf);
		} else 
			directory = argv[1];

		filename = mallocOrExit(strlen(directory) + 100, char);
		seqFilename = mallocOrExit(strlen(directory) + 100, char);
		baseSeqName = mallocOrExit(100, char);

		dir = opendir(directory);

		if (dir == NULL)
			mkdir(directory, 0777);
		else {
			sprintf(buf, "%s/PreGraph", directory);
			remove(buf);
			sprintf(buf, "%s/Graph", directory);
			remove(buf);
			sprintf(buf, "%s/Graph2", directory);
			remove(buf);
			sprintf(buf, "%s/Graph3", directory);
			remove(buf);
			sprintf(buf, "%s/Graph4", directory);
			remove(buf);
		}

		logInstructions(argc, argv, directory);

		strcpy(seqFilename, directory);
		if (isCreateBinary()) {
			// use the CNY unified seq writer
			strcpy(baseSeqName, "/CnyUnifiedSeq");
			// remove other style sequences file
			sprintf(buf, "%s/Sequences", directory);
			remove(buf);
		} else {
			strcpy(baseSeqName, "/Sequences");
			// remove other style sequences file
			sprintf(buf, "%s/CnyUnifiedSeq", directory);
			remove(buf);
			sprintf(buf, "%s/CnyUnifiedSeq.names", directory);
			remove(buf);
		}
		strcat(seqFilename, baseSeqName);

		if ( h == hashLength ) {
			parseDataAndReadFiles(seqFilename, argc - 2, &(argv[2]), &double_strand, &noHash);
		} else {
			sprintf(buf,"rm -f %s",seqFilename);
			if (system(buf)) {
				velvetLog("Command failed!\n");
				velvetLog("%s\n", buf);
#ifdef DEBUG
				abort();
#endif
				exit(1);
			}
			if (argv[1][0] == '/')
				sprintf(buf,"ln -s %s_%d%s %s",argv[1],hashLength,baseSeqName,seqFilename);
			else
				sprintf(buf,"ln -s `pwd`/%s_%d%s %s",argv[1],hashLength,baseSeqName,seqFilename);
			if (system(buf)) {
				velvetLog("Command failed!\n");
				velvetLog("%s\n", buf);
#ifdef DEBUG
				abort();
#endif
				exit(1);
			}
		}

		if (noHash)
			continue;

		splayTable = newSplayTable(h, double_strand);
		if (isCreateBinary()) {
			allSequences = importCnyReadSet(seqFilename);
		} else {
			allSequences = importReadSet(seqFilename);
		}
		velvetLog("%li sequences in total.\n", (long) allSequences->readCount);

		strcpy(filename, directory);
		strcat(filename, "/Roadmaps");
		inputSequenceArrayIntoSplayTableAndArchive(allSequences,
							   splayTable, filename, seqFilename);

		destroySplayTable(splayTable);
		if (dir)
			closedir(dir);
		if (directory != argv[1])
			free(directory);
		free(filename);
		free(seqFilename);
		free(baseSeqName);
		free(buf);
		if (allSequences) {
			destroyReadSet(allSequences);
		}
	}

	return 0;
}
