//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
// ************************************************************************
//@HEADER

#include <stdio.h>
#include <stdlib.h>

#include <Box.hpp>
#include <BoxPartition.hpp>

/*--------------------------------------------------------------------*/

static int box_map_local_entry( const Box& box ,
                                const int ghost ,
                                int local_x ,
                                int local_y ,
                                int local_z )
{
  const int nx = 2 * ghost + box[0][1] - box[0][0] ;
  const int ny = 2 * ghost + box[1][1] - box[1][0] ;
  const int nz = 2 * ghost + box[2][1] - box[2][0] ;
  int result = -1 ;

  local_x += ghost ;
  local_y += ghost ;
  local_z += ghost ;

  if ( 0 <= local_x && local_x < nx &&
       0 <= local_y && local_y < ny &&
       0 <= local_z && local_z < nz ) {

    result = local_z * ny * nx + local_y * nx + local_x ;
  }
  return result ;
}

int box_map_local( const Box& box_local,
                   const int ghost ,
                   const int box_local_map[] ,
                   const int local_x ,
                   const int local_y ,
                   const int local_z )
{
  int result = box_map_local_entry(box_local,ghost,local_x,local_y,local_z);

  if ( 0 <= result ) {
    result = box_local_map[ result ];
  }

  return result ;
}

/*--------------------------------------------------------------------*/
/* Recursively split a box into into (up-ip) sub-boxes */

void box_partition( int ip , int up , int axis ,
                    const Box& box,
                    Box* p_box )
{
  const int np = up - ip ;
  if ( 1 == np ) {
    p_box[ip][0][0] = box[0][0] ; p_box[ip][0][1] = box[0][1] ;
    p_box[ip][1][0] = box[1][0] ; p_box[ip][1][1] = box[1][1] ;
    p_box[ip][2][0] = box[2][0] ; p_box[ip][2][1] = box[2][1] ;
  }
  else {
    const int n = box[ axis ][1] - box[ axis ][0] ;
    const int np_low = np / 2 ;  /* Rounded down */
    const int np_upp = np - np_low ;

    const int n_upp = (int) (((double) n) * ( ((double)np_upp) / ((double)np)));
    const int n_low = n - n_upp ;
    const int next_axis = ( axis + 2 ) % 3 ;

    if ( np_low ) { /* P = [ip,ip+np_low) */
      Box dbox ;
      dbox[0][0] = box[0][0] ; dbox[0][1] = box[0][1] ;
      dbox[1][0] = box[1][0] ; dbox[1][1] = box[1][1] ;
      dbox[2][0] = box[2][0] ; dbox[2][1] = box[2][1] ;

      dbox[ axis ][1] = dbox[ axis ][0] + n_low ;

      box_partition( ip, ip + np_low, next_axis, dbox, p_box );
    }

    if ( np_upp ) { /* P = [ip+np_low,ip+np_low+np_upp) */
      Box dbox;
      dbox[0][0] = box[0][0] ; dbox[0][1] = box[0][1] ;
      dbox[1][0] = box[1][0] ; dbox[1][1] = box[1][1] ;
      dbox[2][0] = box[2][0] ; dbox[2][1] = box[2][1] ;

      ip += np_low ;
      dbox[ axis ][0] += n_low ;
      dbox[ axis ][1]  = dbox[ axis ][0] + n_upp ;

      box_partition( ip, ip + np_upp, next_axis, dbox, p_box );
    }
  }
}

/*--------------------------------------------------------------------*/

static int box_disjoint( const Box& a , const Box& b)
{
  return a[0][1] <= b[0][0] || b[0][1] <= a[0][0] ||
         a[1][1] <= b[1][0] || b[1][1] <= a[1][0] ||
         a[2][1] <= b[2][0] || b[2][1] <= a[2][0] ;
}

static void resize_int( int ** a , int * allocLen , int newLen )
{
  int k = 32;
  while ( k < newLen ) { k <<= 1 ; }
  if ( NULL == *a )
    { *a = (int*)malloc( sizeof(int)*(*allocLen = k) ); }
  else if ( *allocLen < k ) 
    { *a = (int*)realloc(*a , sizeof(int)*(*allocLen = k)); }
}

static void box_partition_maps( 
  const int np ,
  const int my_p ,
  const Box* pbox,
  const int ghost ,
  int ** map_local_id ,
  int ** map_recv_pc ,
  int ** map_send_pc ,
  int ** map_send_id )
{
  const Box& my_box = pbox[my_p] ;

  const int my_ix = my_box[0][0] ;
  const int my_iy = my_box[1][0] ;
  const int my_iz = my_box[2][0] ;
  const int my_nx = my_box[0][1] - my_box[0][0] ;
  const int my_ny = my_box[1][1] - my_box[1][0] ;
  const int my_nz = my_box[2][1] - my_box[2][0] ;

  const int my_use_nx = 2 * ghost + my_nx ;
  const int my_use_ny = 2 * ghost + my_ny ;
  const int my_use_nz = 2 * ghost + my_nz ;

  const int id_length = my_use_nx * my_use_ny * my_use_nz ;

  int * local_id  = (int *) malloc( id_length * sizeof(int) );
  int * recv_pc   = (int *) malloc( ( np + 1 ) * sizeof(int) );
  int * send_pc   = (int *) malloc( ( np + 1 ) * sizeof(int) );

  int * send_id  = NULL ;
  int   send_id_size = 0 ;

  int iLocal , iSend ;
  int i ;

  Box my_use_box;

  my_use_box[0][0] = my_box[0][0] - ghost ;
  my_use_box[0][1] = my_box[0][1] + ghost ;
  my_use_box[1][0] = my_box[1][0] - ghost ;
  my_use_box[1][1] = my_box[1][1] + ghost ;
  my_use_box[2][0] = my_box[2][0] - ghost ;
  my_use_box[2][1] = my_box[2][1] + ghost ;

  for ( i = 0 ; i < id_length ; ++i ) { local_id[i] = -1 ; }

  iSend = 0 ;
  iLocal = 0 ;

  /* The vector space is partitioned by processors */

  for ( i = 0 ; i < np ; ++i ) {
    const int ip = ( i + my_p ) % np ;
    recv_pc[i] = iLocal ;
    send_pc[i] = iSend ;

    if ( ! box_disjoint( my_use_box , pbox[ip] ) ) {
      const int p_ix = pbox[ip][0][0] ;
      const int p_iy = pbox[ip][1][0] ;
      const int p_iz = pbox[ip][2][0] ;
      const int p_ex = pbox[ip][0][1] ;
      const int p_ey = pbox[ip][1][1] ;
      const int p_ez = pbox[ip][2][1] ;

      int local_x , local_y , local_z ;

      /* Run the span of global cells that my processor uses */

      for ( local_z = -ghost ; local_z < my_nz + ghost ; ++local_z ) {
      for ( local_y = -ghost ; local_y < my_ny + ghost ; ++local_y ) {
      for ( local_x = -ghost ; local_x < my_nx + ghost ; ++local_x ) {

        const int global_z = local_z + my_iz ;
        const int global_y = local_y + my_iy ;
        const int global_x = local_x + my_ix ;

        const int entry = 
          box_map_local_entry(my_box,ghost,local_x,local_y,local_z);

        if ( entry < 0 ) { abort(); }

        if ( p_iz <= global_z && global_z < p_ez &&
             p_iy <= global_y && global_y < p_ey &&
             p_ix <= global_x && global_x < p_ex ) {

          /* This ordinal is owned by processor 'ip' */

          local_id[ entry ] = iLocal++ ;

#if defined(DEBUG_PRINT)
if ( my_p != ip ) {
  fprintf(stdout,"  (%d,%d,%d) : P%d recv at local %d from P%d\n",
                  global_x,global_y,global_z,my_p,local_id[entry],ip);
  fflush(stdout);
}
#endif
        }

        /* If in my ownership and used by the other processor */
        if ( my_p != ip &&
             /* In my ownership: */
             ( 0 <= local_z && local_z < my_nz &&
               0 <= local_y && local_y < my_ny &&
               0 <= local_x && local_x < my_nx ) &&
             /* In other processors usage: */
             ( p_iz - ghost <= global_z && global_z < p_ez + ghost &&
               p_iy - ghost <= global_y && global_y < p_ey + ghost &&
               p_ix - ghost <= global_x && global_x < p_ex + ghost ) ) {

          resize_int( & send_id , & send_id_size , (iSend + 1) );
          send_id[ iSend ] = local_id[ entry ] ;
          ++iSend ;

#if defined(DEBUG_PRINT)
{
  fprintf(stdout,"  (%d,%d,%d) : P%d send at local %d to P%d\n",
                  global_x,global_y,global_z,my_p,local_id[entry],ip);
  fflush(stdout);
}
#endif
        }
      }
    }
    }
    }
  }
  recv_pc[np] = iLocal ;
  send_pc[np] = iSend ;

  *map_local_id  = local_id ;
  *map_recv_pc   = recv_pc ;
  *map_send_pc   = send_pc ;
  *map_send_id   = send_id ;
}

void box_partition_rcb( const int np , 
                        const int my_p ,
                        const Box& root_box,
                        const int ghost ,
                        Box** pbox,
                        int ** map_local_id ,
                        int ** map_recv_pc ,
                        int ** map_send_pc ,
                        int ** map_send_id )
{
  *pbox = new Box[ np ];

  box_partition( 0 , np , 2 , root_box , *pbox );

  box_partition_maps( np , my_p , *pbox , ghost ,
                      map_local_id , map_recv_pc , 
                      map_send_pc , map_send_id );
}

/*--------------------------------------------------------------------*/

#ifdef UNIT_TEST

static int box_contain( const Box& a , const Box& b )
{
  return a[0][0] <= b[0][0] && b[0][1] <= a[0][1] &&
         a[1][0] <= b[1][0] && b[1][1] <= a[1][1] &&
         a[2][0] <= b[2][0] && b[2][1] <= a[2][1] ;
}

static void box_print( FILE * fp , const Box& a )
{
  fprintf(fp,"{ [ %d , %d ) , [ %d , %d ) , [ %d , %d ) }",
                a[0][0] , a[0][1] ,  
                a[1][0] , a[1][1] ,  
                a[2][0] , a[2][1] );
}

static void test_box( const Box& box , const int np )
{
  const int ncell_box = box[0][1] * box[1][1] * box[2][1] ;
  int ncell_total = 0 ;
  int ncell_min = ncell_box ;
  int ncell_max = 0 ;
  std::vector<Box> pbox(np);
  int i , j ;

  box_partition( 0 , np , 2 , box , &pbox[0] );

  for ( i = 0 ; i < np ; ++i ) {
    const int ncell = ( pbox[i][0][1] - pbox[i][0][0] ) *
                      ( pbox[i][1][1] - pbox[i][1][0] ) *
                      ( pbox[i][2][1] - pbox[i][2][0] );

    if ( ! box_contain( box , pbox[i] ) ) {
      fprintf(stdout,"  OUT OF BOUNDS pbox[%d/%d] = ",i,np);
      box_print(stdout,pbox[i]);
      fprintf(stdout,"\n");
      abort();
    }

    for ( j = i + 1 ; j < np ; ++j ) {
      if ( ! box_disjoint( pbox[i] , pbox[j] ) ) {
        fprintf(stdout,"  NOT DISJOINT pbox[%d/%d] = ",i,np);
        box_print(stdout, pbox[i]);
        fprintf(stdout,"\n");
        fprintf(stdout,"               pbox[%d/%d] = ",j,np);
        box_print(stdout, pbox[j]);
        fprintf(stdout,"\n");
        abort();
      }
    }
    ncell_total += ncell ;

    if ( ncell_max < ncell ) { ncell_max = ncell ; }
    if ( ncell < ncell_min ) { ncell_min = ncell ; }
  }

  if ( ncell_total != ncell_box ) {
    fprintf(stdout,"  WRONG CELL COUNT NP = %d\n",np);
    abort();
  }
  fprintf(stdout,"NP = %d, total = %d, avg = %d, min = %d, max = %d\n",
          np,ncell_box,ncell_box/np,ncell_min,ncell_max);
}

/*--------------------------------------------------------------------*/

static void test_maps( const Box& root_box , const int np )
{
  const int ghost = 1 ;
  const int nx_global = root_box[0][1] - root_box[0][0] ;
  const int ny_global = root_box[1][1] - root_box[1][0] ;
  int ieq , i , j ;
  std::vector<Box> pbox(np);
  int **local_values ;
  int **map_local_id ;
  int **map_recv_pc ;
  int **map_send_pc ;
  int **map_send_id ;

  box_partition( 0 , np , 2 , root_box , &pbox[0] );

  local_values = (int **) malloc( sizeof(int*) * np );
  map_local_id = (int **) malloc( sizeof(int*) * np );
  map_recv_pc  = (int **) malloc( sizeof(int*) * np );
  map_send_pc  = (int **) malloc( sizeof(int*) * np );
  map_send_id  = (int **) malloc( sizeof(int*) * np );

  /* Set each local value to the global equation number */

  for ( ieq = i = 0 ; i < np ; ++i ) {
    const Box& mybox = pbox[i] ;
    const int nx = mybox[0][1] - mybox[0][0] ;
    const int ny = mybox[1][1] - mybox[1][0] ;
    const int nz = mybox[2][1] - mybox[2][0] ;
    int ix , iy , iz ;

    /* Generate the partition maps for this rank */
    box_partition_maps( np , i , &pbox[0] , ghost ,
                        & map_local_id[i] , & map_recv_pc[i] , 
                        & map_send_pc[i] , & map_send_id[i] );

    local_values[i] = (int *) malloc( sizeof(int) * map_recv_pc[i][np] );

    for ( iz = -ghost ; iz < nz + ghost ; ++iz ) {
    for ( iy = -ghost ; iy < ny + ghost ; ++iy ) {
    for ( ix = -ghost ; ix < nx + ghost ; ++ix ) {
      const int ieq = box_map_local(mybox,ghost,map_local_id[i],ix,iy,iz);

      if ( 0 <= ieq ) {
        const int ix_global = ix + mybox[0][0] ;
        const int iy_global = iy + mybox[1][0] ;
        const int iz_global = iz + mybox[2][0] ;

        if ( root_box[0][0] <= ix_global && ix_global < root_box[0][1] &&
             root_box[1][0] <= iy_global && iy_global < root_box[1][1] &&
             root_box[2][0] <= iz_global && iz_global < root_box[2][1] ) {

          local_values[i][ ieq ] = ix_global +
                                   iy_global * nx_global +
                                   iz_global * nx_global * ny_global ;
        }
        else {
          local_values[i][ ieq ] = -1 ;
        }
      }
    }
    }
    }
  }

  /* Pair-wise compare the local values */
  /* i  == receiving processor rank */
  /* ip == sending   processor rank */
  /* j  == receiving processor data entry for message from 'ip' */
  /* jp == sending   processor data entry for message to   'i' */

  for ( i = 0 ; i < np ; ++i ) {
    for ( j = 1 ; j < np ; ++j ) {
      const int ip = ( i + j ) % np ;
      const int jp = ( i + np - ip ) % np ;
      const int nrecv = map_recv_pc[i] [j+1]  - map_recv_pc[i] [j] ;
      const int nsend = map_send_pc[ip][jp+1] - map_send_pc[ip][jp] ;
      int k ;
      if ( nrecv != nsend ) {
        fprintf(stderr,"P%d recv %d from P%d\n",i,nrecv,ip);
        fprintf(stderr,"P%d send %d to   P%d\n",ip,nsend,i);
        abort();
      }
      for ( k = 0 ; k < nrecv ; ++k ) {
        const int irecv = map_recv_pc[i][j] + k ;
        const int isend = map_send_pc[ip][jp] + k ;
        const int val_irecv = local_values[i][irecv] ;
        const int val_isend = local_values[ip][ map_send_id[ip][isend] ] ;
        if ( val_irecv != val_isend ) {
          fprintf(stderr,"P%d recv[%d] = %d , from P%d\n",i,k,val_irecv,ip);
          fprintf(stderr,"P%d send[%d] = %d , to   P%d\n",ip,k,val_isend,i);
          abort();
        }
      }
    }
  }

  for ( i = 0 ; i < np ; ++i ) {
    free( map_local_id[i] );
    free( map_recv_pc[i] );
    free( map_send_pc[i] );
    free( map_send_id[i] );
    free( local_values[i] );
  }
  free( map_send_id );
  free( map_send_pc );
  free( map_recv_pc );
  free( map_local_id );
  free( local_values );
}

/*--------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
  int np_max = 256 ;
  Box box = { 0 , 64 , 0 , 64 , 0 , 64 };
  int np = 0 ;

  switch( argc ) {
  case 3:
    sscanf(argv[1],"%d",&np);
    sscanf(argv[2],"%dx%dx%d",& box[0][1] , & box[1][1] , & box[2][1] );
    if ( 0 < np ) { test_box( box , np ); }
    if ( 0 < np ) { test_maps( box , np ); }
    break ;
  default:
    for ( np = 1 ; np <= np_max ; ++np ) {
      test_box( box , np );
      test_maps( box , np );
    }
    break ;
  }
  return 0 ;
}

#endif


