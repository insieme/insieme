//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB SP code for multiple    //
//  devices. This OpenCL version is developed by the Center for Manycore   //
//  Programming at Seoul National University and derived from the MPI      //
//  Fortran versions in "NPB3.3-MPI" developed by NAS.                     //
//                                                                         //
//  Permission to use, copy, distribute and modify this software for any   //
//  purpose with or without fee is hereby granted. This software is        //
//  provided "as is" without express or implied warranty.                  //
//                                                                         //
//  Information on NPB 3.3, including the technical report, the original   //
//  specifications, source code, results and information on how to submit  //
//  new results, is available at:                                          //
//                                                                         //
//           http://www.nas.nasa.gov/Software/NPB/                         //
//                                                                         //
//  Send comments or suggestions for this OpenCL version to                //
//  cmp@aces.snu.ac.kr                                                     //
//                                                                         //
//          Center for Manycore Programming                                //
//          School of Computer Science and Engineering                     //
//          Seoul National University                                      //
//          Seoul 151-744, Korea                                           //
//                                                                         //
//          E-mail:  cmp@aces.snu.ac.kr                                    //
//                                                                         //
//-------------------------------------------------------------------------//

//-------------------------------------------------------------------------//
// Authors: Sangmin Seo, Jungwon Kim, Jun Lee, Gangwon Jo, Jeongho Nah,    //
//          and Jaejin Lee                                                 //
//-------------------------------------------------------------------------//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "header.h"

//---------------------------------------------------------------------
// This function allocates space for a set of cells and fills the set     
// such that communication between cells on different nodes is only
// nearest neighbor                                                   
//---------------------------------------------------------------------
void make_set()
{
  int k, p, i, j, c, dir, size, excess;

  //---------------------------------------------------------------------
  // this makes coding easier
  //---------------------------------------------------------------------
  p = ncells;

  for (k = 0; k < p * p; k++) {
    //---------------------------------------------------------------------
    // determine the location of the cell at the bottom of the 3D 
    // array of cells
    //---------------------------------------------------------------------
    cell_coord[k][0][0] = k % p;
    cell_coord[k][0][1] = k / p;
    cell_coord[k][0][2] = 0;

    //---------------------------------------------------------------------
    // set the cell_coords for cells in the rest of the z-layers; 
    // this comes down to a simple linear numbering in the z-direct-
    // ion, and to the doubly-cyclic numbering in the other dirs     
    //---------------------------------------------------------------------
    for (c = 1; c < p; c++) {
      cell_coord[k][c][0] = (cell_coord[k][c-1][0]+1) % p; 
      cell_coord[k][c][1] = (cell_coord[k][c-1][1]-1+p) % p;
      cell_coord[k][c][2] = c;
    }

    //---------------------------------------------------------------------
    // slice[n][dir] contains the sequence number of the cell that is in
    // coordinate plane n in the dir direction
    //---------------------------------------------------------------------
    for (c = 0; c < p; c++) {
      for (dir = 0; dir < 3; dir++) {
        slice[k][cell_coord[k][c][dir]][dir] = c;
      }
    }


    //---------------------------------------------------------------------
    // fill the predecessor and successor entries, using the indices 
    // of the bottom cells (they are the same at each level of k 
    // anyway) acting as if full periodicity pertains; note that p is
    // added to those arguments to the mod functions that might
    // otherwise return wrong values when using the modulo function
    //---------------------------------------------------------------------
    i = cell_coord[k][0][0];
    j = cell_coord[k][0][1];

    predecessor[k][0] = ((i-1+p) % p) + p*j;
    predecessor[k][1] = i + p*((j-1+p) % p);
    predecessor[k][2] = ((i+1) % p) + p*((j-1+p) % p);
    successor[k][0]   = ((i+1) % p) + p*j;
    successor[k][1]   = i + p*((j+1) % p);
    successor[k][2]   = ((i-1+p) % p) + p*((j+1) % p);

    //---------------------------------------------------------------------
    // now compute the sizes of the cells                                    
    //---------------------------------------------------------------------
    for (dir = 0; dir < 3; dir++) {
      //---------------------------------------------------------------------
      // set cell_coord range for each direction
      //---------------------------------------------------------------------
      size   = grid_points[dir] / p;
      excess = grid_points[dir] % p;
      for (c = 0; c < ncells; c++) {
        if (cell_coord[k][c][dir] < excess) {
          cell_size[k][c][dir] = size+1;
          cell_low[k][c][dir]  = cell_coord[k][c][dir]*(size+1);
          cell_high[k][c][dir] = cell_low[k][c][dir]+size;
        } else { 
          cell_size[k][c][dir] = size;
          cell_low[k][c][dir]  = excess*(size+1)+
                              (cell_coord[k][c][dir]-excess)*size;
          cell_high[k][c][dir] = cell_low[k][c][dir]+size-1;
        }
        if (cell_size[k][c][dir] <= 2) {
          printf(" Error: Cell size too small. Min size is 3\n");
          exit(EXIT_FAILURE);
        }
      }
    }

    for (c = 0; c < ncells; c++) {
      if ( (cell_size[k][c][0] > IMAX) ||
           (cell_size[k][c][1] > JMAX) ||
           (cell_size[k][c][2] > KMAX) ) {
        printf("device=%d c=%d, cell_size-(%d, %d, %d)\n",
            k, c, cell_size[k][c][0], cell_size[k][c][1], cell_size[k][c][2]);
        printf(" Problem size too big for compiled array sizes\n");
        exit(EXIT_FAILURE);
      }
    }
  }
}

