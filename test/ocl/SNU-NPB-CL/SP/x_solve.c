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
// step in the x-direction for all five matrix components
// simultaneously. The Thomas algorithm is employed to solve the
// systems for the x-lines. Boundary conditions are non-periodic
//---------------------------------------------------------------------
void x_solve()
{
  int stage, i, c, buffer_size;
  cl_int ecode = 0;

  //---------------------------------------------------------------------
  // OK, now we know that there are multiple processors
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // now do a sweep on a layer-by-layer basis, i.e. sweeping through cells
  // on this node in the direction of increasing i for the forward sweep,
  // and after that reversing the direction for the backsubstitution.
  //---------------------------------------------------------------------

  if (timeron) timer_start(t_xsolve);
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
                                       k_lhsx[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       lhsx_gw[i][slice[i][stage-1][0]],
                                       lhsx_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for lhsx");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      //---------------------------------------------------------------------
      // wait for pending communication to complete
      // This waits on the current receive and on the send
      // from the previous stage. They always come in pairs. 
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2 + 1);
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_x_solve1[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       x_solve1_gw[i][slice[i][stage-1][0]],
                                       x_solve1_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for x_solve1");
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
                                       k_lhsx[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       lhsx_gw[i][slice[i][stage-1][0]],
                                       lhsx_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for lhsx");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    }

    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_x_solve2[i][slice[i][stage-1][0]],
                                     2, NULL,
                                     x_solve2_gw[i][slice[i][stage-1][0]],
                                     x_solve2_lw[i][slice[i][stage-1][0]],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for x_solve2");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }

    //---------------------------------------------------------------------
    // send information to the next processor, except when this
    // is the last grid block
    //---------------------------------------------------------------------
    if (stage != ncells) {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_x_solve3[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       x_solve3_gw[i][slice[i][stage-1][0]],
                                       x_solve3_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for x_solve3");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

      //---------------------------------------------------------------------
      // send data to next phase
      // can't receive data yet because buffer size will be wrong 
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);

        c      = slice[i][stage-1][0];
        buffer_size = (cell_size[i][c][1]-cell_start[i][c][1]-cell_end[i][c][1]) * 
                      (cell_size[i][c][2]-cell_start[i][c][2]-cell_end[i][c][2]);

        ecode = clEnqueueCopyBuffer(cmd_queue[successor[i][0] * 2 + 1],
                                    m_out_buffer[i],
                                    m_in_buffer[successor[i][0]],
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
      // while waiting, do the block-diagonal inversion for the 
      // cell that was just finished                
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_ninvr[i][slice[i][stage][0]],
                                       NINVR_DIM, NULL,
                                       ninvr_gw[i][slice[i][stage][0]],
                                       ninvr_lw[i][slice[i][stage][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for ninvr");
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
                                       k_x_solve4[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       x_solve4_gw[i][slice[i][stage-1][0]],
                                       x_solve4_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for x_solve4");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }

    } else {
      for (i = 0; i < num_devices; i++) {
        ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                       k_x_solve5[i][slice[i][stage-1][0]],
                                       2, NULL,
                                       x_solve5_gw[i][slice[i][stage-1][0]],
                                       x_solve5_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for x_solve5");
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
                                       k_x_solve6[i][slice[i][stage-1][0]],
                                       X_SOLVE6_DIM, NULL,
                                       x_solve6_gw[i][slice[i][stage-1][0]],
                                       x_solve6_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for x_solve6");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
      //---------------------------------------------------------------------
      // pack and send the buffer
      //---------------------------------------------------------------------
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);

        c      = slice[i][stage-1][0];
        buffer_size = (cell_size[i][c][1]-cell_start[i][c][1]-cell_end[i][c][1]) * 
                      (cell_size[i][c][2]-cell_start[i][c][2]-cell_end[i][c][2]);

        ecode = clEnqueueCopyBuffer(cmd_queue[predecessor[i][0] * 2 + 1],
                                    m_out_buffer[i],
                                    m_in_buffer[predecessor[i][0]],
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
                                       k_ninvr[i][slice[i][stage-1][0]],
                                       NINVR_DIM, NULL,
                                       ninvr_gw[i][slice[i][stage-1][0]],
                                       ninvr_lw[i][slice[i][stage-1][0]],
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRange() for ninvr");
      }
      for (i = 0; i < num_devices; i++) {
        CHECK_FINISH(i * 2);
      }
    }
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);

  if (timeron) timer_stop(t_xsolve);
}

