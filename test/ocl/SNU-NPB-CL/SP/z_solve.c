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

#include "header.h"

//---------------------------------------------------------------------
// this function performs the solution of the approximate factorization
// step in the z-direction for all five matrix components
// simultaneously. The Thomas algorithm is employed to solve the
// systems for the z-lines. Boundary conditions are non-periodic
//---------------------------------------------------------------------
void z_solve()
{
  int stage, i, c, buffer_size;
  cl_int ecode = 0;

  //---------------------------------------------------------------------
  // now do a sweep on a layer-by-layer basis, i.e. sweeping through cells
  // on this node in the direction of increasing i for the forward sweep,
  // and after that reversing the direction for the backsubstitution  
  //---------------------------------------------------------------------

  if (timeron) timer_start(t_zsolve);
  //---------------------------------------------------------------------
  // FORWARD ELIMINATION  
  //---------------------------------------------------------------------
  for (stage = 1; stage <= ncells; stage++) {
    if (stage != 1) {
      //---------------------------------------------------------------------
      // communication has already been started. 
      // compute the left hand side while waiting for the msg
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_lhsz[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       lhsz_gw[i][slice[i][stage-1][2]],
                                       lhsz_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for lhsz");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      //---------------------------------------------------------------------
      // wait for pending communication to complete
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2 + 1);
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_z_solve1[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       z_solve1_gw[i][slice[i][stage-1][2]],
                                       z_solve1_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for z_solve1");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    } else {            
      //---------------------------------------------------------------------
      // if this IS the first cell, we still compute the lhs
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_lhsz[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       lhsz_gw[i][slice[i][stage-1][2]],
                                       lhsz_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for lhsz");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    }

    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_z_solve2[i][slice[i][stage-1][2]],
                                     2, NULL,
                                     z_solve2_gw[i][slice[i][stage-1][2]],
                                     z_solve2_lw[i][slice[i][stage-1][2]],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for z_solve2");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }

    //---------------------------------------------------------------------
    // send information to the next processor, except when this
    // is the last grid block,
    //---------------------------------------------------------------------

    if (stage != ncells) {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_z_solve3[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       z_solve3_gw[i][slice[i][stage-1][2]],
                                       z_solve3_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for z_solve3");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);

        c      = slice[i][stage-1][2];
        buffer_size = (cell_size[i][c][0]-cell_start[i][c][0]-cell_end[i][c][0]) * 
                      (cell_size[i][c][1]-cell_start[i][c][1]-cell_end[i][c][1]);

        ecode = clEnqueueCopyBuffer(cmd_queue[successor[i][2] * 2 + 1],
                                    m_out_buffer[i],
                                    m_in_buffer[successor[i][2]],
                                    0, 0, 22*buffer_size*sizeof(double),
                                    0, NULL, NULL);
      }
    }
  }

  //---------------------------------------------------------------------
  // now go in the reverse direction                      
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // BACKSUBSTITUTION 
  //---------------------------------------------------------------------
  for (stage = ncells; stage >= 1; stage--) {
    if (stage != ncells) {
      //---------------------------------------------------------------------
      // communication has already been started
      // while waiting, do the  block-diagonal inversion for the 
      // cell that was just finished                
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_tzetar[i][slice[i][stage][2]],
                                       TZETAR_DIM, NULL,
                                       tzetar_gw[i][slice[i][stage][2]],
                                       tzetar_lw[i][slice[i][stage][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for tzetar");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      //---------------------------------------------------------------------
      // wait for pending communication to complete
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2 + 1);
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_z_solve4[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       z_solve4_gw[i][slice[i][stage-1][2]],
                                       z_solve4_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for z_solve4");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

    } else {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_z_solve5[i][slice[i][stage-1][2]],
                                       2, NULL,
                                       z_solve5_gw[i][slice[i][stage-1][2]],
                                       z_solve5_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for z_solve5");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    }

    //---------------------------------------------------------------------
    // send on information to the previous processor, if needed
    //---------------------------------------------------------------------
    if (stage != 1) {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_z_solve6[i][slice[i][stage-1][2]],
                                       Z_SOLVE6_DIM, NULL,
                                       z_solve6_gw[i][slice[i][stage-1][2]],
                                       z_solve6_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for z_solve6");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);

        c      = slice[i][stage-1][2];
        buffer_size = (cell_size[i][c][0]-cell_start[i][c][0]-cell_end[i][c][0]) * 
                      (cell_size[i][c][1]-cell_start[i][c][1]-cell_end[i][c][1]);

        ecode = clEnqueueCopyBuffer(cmd_queue[predecessor[i][2] * 2 + 1],
                                    m_out_buffer[i],
                                    m_in_buffer[predecessor[i][2]],
                                    0, 0, 10*buffer_size*sizeof(double),
                                    0, NULL, NULL);
      }
    }

    //---------------------------------------------------------------------
    // If this was the last stage, do the block-diagonal inversion
    //---------------------------------------------------------------------
    if (stage == 1) {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_tzetar[i][slice[i][stage-1][2]],
                                       TZETAR_DIM, NULL,
                                       tzetar_gw[i][slice[i][stage-1][2]],
                                       tzetar_lw[i][slice[i][stage-1][2]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for tzetar");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    }
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);

  if (timeron) timer_stop(t_zsolve);
}

