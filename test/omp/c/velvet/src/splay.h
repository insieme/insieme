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
#ifndef _Splay_H
#define _Splay_H

#include <stdio.h>

typedef struct splayNode_st SplayTree;

// Deallocates tree memory
void destroyAllSplayTrees();

// Finds occurrence of kmer in the tree
// If found, returns TRUE, and seqID and coordinate are accordingly modified
// If not, a new leaf is added to the tree, with the seqID and position data
boolean findOrInsertOccurenceInSplayTree(Kmer * kmer, IDnum * seqID,
					 Coordinate * position,
					 SplayTree ** T);

#ifdef _OPENMP
/* Initialises the per-thread RecycleBin array */
void initSplayTreeMemory(void);
#endif

#endif
