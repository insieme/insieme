#ifndef _perform_element_loop_hpp_
#define _perform_element_loop_hpp_

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

#include <BoxIterator.hpp>
#include <simple_mesh_description.hpp>
#include <SparseMatrix_functions.hpp>
#include <box_utils.hpp>
#include <Hex8_box_utils.hpp>
#include <Hex8_ElemData.hpp>

#ifdef _OPENMP
#include <omp.h>
#endif

//#include "advisor-annotate.h"

namespace miniFE {

template<typename GlobalOrdinal,
         typename MatrixType, typename VectorType>
void
perform_element_loop(const simple_mesh_description<GlobalOrdinal>& mesh,
                     const Box& local_elem_box,
                     MatrixType& A, VectorType& b,
                     Parameters& /*params*/)
{
  typedef typename MatrixType::ScalarType Scalar;

  int global_elems_x = mesh.global_box[0][1];
  int global_elems_y = mesh.global_box[1][1];
  int global_elems_z = mesh.global_box[2][1];

  //We will iterate the local-element-box (local portion of the mesh), and
  //get element-IDs in preparation for later assembling the FE operators
  //into the global sparse linear-system.

  GlobalOrdinal num_elems = get_num_ids<GlobalOrdinal>(local_elem_box);
  std::vector<GlobalOrdinal> elemIDs(num_elems);

  BoxIterator iter = BoxIterator::begin(local_elem_box);
  BoxIterator end  = BoxIterator::end(local_elem_box);

  for(size_t i=0; iter != end; ++iter, ++i) {
    elemIDs[i] = get_id<GlobalOrdinal>(global_elems_x, global_elems_y, global_elems_z,
                                       iter.x, iter.y, iter.z);
  }

  timer_type t_gn = 0, t_ce = 0, t_si = 0;
  timer_type t0 = 0;

  #pragma omp parallel for shared (elemIDs)
  for(MINIFE_GLOBAL_ORDINAL i=0; i < elemIDs.size(); ++i) {
    ElemData<GlobalOrdinal,Scalar> elem_data;
    compute_gradient_values(elem_data.grad_vals);

    get_elem_nodes_and_coords(mesh, elemIDs[i], elem_data);
    compute_element_matrix_and_vector(elem_data);
    sum_into_global_linear_system(elem_data, A, b);
  }

//std::cout << std::endl<<"get-nodes: " << t_gn << std::endl;
//std::cout << "compute-elems: " << t_ce << std::endl;
//std::cout << "sum-in: " << t_si << std::endl;
}

}//namespace miniFE

#endif

