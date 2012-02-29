/*************************************************************************/
/*                                                                       */
/* Licensed Materials - Property of IBM                                  */
/*                                                                       */
/* (C) Copyright IBM Corp. 2010                                          */
/* All Rights Reserved                                                   */
/*                                                                       */
/* US Government Users Restricted Rights - Use, duplication or           */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.     */
/*                                                                       */
/*************************************************************************/

/**************************************************************************
 Defines, Macros and Types
 **************************************************************************/
#ifdef INSIEME
#include "ocl_device.h"
#endif


typedef float value_type;

/* ghost cell width */
#define GHOST_CELL_WIDTH   1

/* define some macros to get access to elements for this workgroup tile */
#define LOCAL_INDEX        (get_local_id(0) * get_local_size(1) + get_local_id(1))
#define GROUP_INDEX        (get_group_id(0) * get_num_groups(1) + get_group_id(1))

/* check for a specific work item, in this case 0,0 */
#define SERIALIZED_ITEM    (0 == get_local_id(0)) && (0 == get_local_id(1))


/**************************************************************************
 Function: load_tile_cache

 Loads the tile cache in local memory from device memory.

 params:
    a_old            input array of old values
    tile_cache       workgroup tile (plus ghost cells) to cache array values
    height           y stride of input and output arrays
 **************************************************************************/
void load_tile_cache(const __global value_type *a_old,
                     __local value_type *tile_cache,
                     const uint height) {

    /* get workgroup indices and add 1 for the correct node position - skip boundary */
    uint xstride = (get_global_id(0) + 1) * height;
    uint y = get_global_id(1) + 1;

    uint i = get_local_id(0) + 1;
    uint j = get_local_id(1) + 1;
    uint jheight = get_local_size(1)+2*GHOST_CELL_WIDTH;
    uint istride = i * jheight;

    /* copy into local memory */
    tile_cache[istride + j] = a_old[xstride + y];

    /* first column */
    if (0 == get_local_id(0)) {
        tile_cache[j] = a_old[xstride - height + y];
    }
    /* last column */
    if (i == get_local_size(0)) {
        tile_cache[istride + jheight + j] = a_old[xstride + height + y];
    }
    /* first row */
    if (0 == get_local_id(1)) {
        tile_cache[istride] = a_old[xstride + y - 1];
    }
    /* last row */
    if (get_local_size(1) == get_local_id(1)+1) {
        tile_cache[istride + j + 1] = a_old[xstride + y + 1];
    }

    /* wait until all rows complete */
    barrier(CLK_LOCAL_MEM_FENCE);
}


/**************************************************************************
 Function: local_stencil_calc

 Calculates 5-point stencil and difference from local store cache

 params:
    tile_cache       workgroup tile (plus ghost cells) to cache array values 
    a_new            output array of calculated values
    tile_deltas      workgroup tile to hold differences between old and new values    
    height           y stride of input and output arrays
 **************************************************************************/
void local_stencil_calc(__local value_type *tile_cache,
                        __global value_type *a_new,
                        __local value_type *tile_deltas,
                        const uint height) {

    /* get workgroup indices and add 1 for the correct node position - skip boundary */
    uint xstride = (get_global_id(0) + 1) * height;
    uint y = get_global_id(1) + 1;

    /* get cache indices and add 1 for the correct node position - skip boundary */
    uint i = get_local_id(0) + 1;
    uint j = get_local_id(1) + 1;
    uint jheight = get_local_size(1)+2*GHOST_CELL_WIDTH;
    uint istride = i * jheight;

    /* Jacobi iteration - simple 5 point stencil in 2 dimensions */
    value_type new_value = 0.25f * (tile_cache[istride - jheight + j] +
				    tile_cache[istride + jheight + j] +
				    tile_cache[istride + j - 1] +
				    tile_cache[istride + j + 1]);
    a_new[xstride + y] = new_value;

    /* store delta value for this node in local buffer per workgroup */
    tile_deltas[LOCAL_INDEX] = fabs(new_value - tile_cache[istride + j]);

    /* wait until whole tile is filled with the delta values */
    barrier(CLK_LOCAL_MEM_FENCE);
}


/**************************************************************************
 Function: two_stage_reduction

 Calculate the maximum delta for this tile. This is a serialized
 process that requires atomic access to the tile of delta values.
 Use two stage reduction, one across rows and then down one column

 params:
    tile_deltas       workgroup tile to hold differences between old and new values
    deltas            workgroup results, one cell per workgroup
 **************************************************************************/
void two_stage_reduction(__local value_type *tile_deltas,
                         __global value_type *deltas) {

    /* scan across row for given get_local_id(1) index finding the maximum */
    if (0 == get_local_id(0)) {
        uint i, j = get_local_id(1);
        uint istride = j;

        for (i=1; i< get_local_size(0); i++) {
            istride += get_local_size(1);
            tile_deltas[j] = fmax(tile_deltas[j], tile_deltas[istride]);
        }
    }

    /* wait until all rows complete */
    barrier(CLK_LOCAL_MEM_FENCE);

    /* find workblock maximum using one specific work item */
    if (SERIALIZED_ITEM) {
        uint j;
        value_type max_delta = 0.0f;

        /* loop over first column of delta values to find maximum */
        for (j=0; j< get_local_size(1); j++) {
                max_delta = fmax(max_delta, tile_deltas[j]);
        }

        /* update delta value for this workgroup */
        deltas[GROUP_INDEX] = max_delta;
    }
}



/**************************************************************************
 Function: ocl_jacobi_local_copy

 Same as ocl_jacobi_async_copy but uses workitem copy to load local memory

 **************************************************************************/
#ifdef INSIEME
#pragma insieme mark
#endif
__kernel
void ocl_jacobi_local_copy(const __global value_type *a_old,
                           __global value_type *a_new,
                           __local value_type *tile_deltas,
                           __local value_type *tile_cache,
                           __global value_type *deltas,
                           const uint height)
{
    /* cache values in local memory */
    load_tile_cache(a_old, tile_cache, height);

    /* calculate the stencil and difference between new and old value */
    local_stencil_calc(tile_cache, a_new, tile_deltas, height);

    /* Use two stage reduction to calculate the maximum delta for this tile */
    two_stage_reduction(tile_deltas, deltas);
}

