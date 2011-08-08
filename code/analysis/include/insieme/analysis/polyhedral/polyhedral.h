/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#pragma once 

#include <array>
#include <iterator>
#include <stdexcept>
#include <memory>
#include <set>
#include <list>

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/constraint.h"
#include "insieme/core/ast_node.h"
#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"
#include "boost/mpl/or.hpp"

namespace insieme {
namespace analysis {
namespace poly {

// Defines a list of constraints stored in a vector
typedef std::vector<Constraint> ConstraintList;

//****************************************************************************************************
// IterationDomanin: is the class which defines the shape of the polyhedron, the set of integer
// points of an N-dimensional plane are delimited by affine linear functions which define a convex
// region, therefore the polyhedron. 
//****************************************************************************************************
struct IterationDomain : public utils::Printable {
	IterationDomain(const IterationVector& iterVec, const ConstraintCombinerPtr& combiner) : 
		iterVec(iterVec), constraints(combiner) { }

	std::ostream& printTo(std::ostream& out) const {
		return out << "Iteration Domain: \n\tIV: " << iterVec << "\n\tCONS: [ " << *constraints << " ]";
	}

private:
	IterationVector 		iterVec;
	ConstraintCombinerPtr	constraints; 
};

//*****************************************************************************************************
// ScatteringFunction: A scattering function represent the order of execution of statements inside a
// SCoP
//*****************************************************************************************************
struct ScatteringFunction : public utils::Printable {

	ScatteringFunction(const IterationVector& iterVec) : iterVec(iterVec) { }
	ScatteringFunction(const ScatteringFunction& other) : iterVec(other.iterVec) { cloneRows(other.funcs); }

	inline void appendRow(const AffineFunction& af) {  funcs.push_back( af.toBase(iterVec) ); }
	
	ScatteringFunction& operator=(const ScatteringFunction& other);

	inline const IterationVector& getIterationVector() const { return iterVec; }

	std::ostream& printTo(std::ostream& out) const;

private:

	void cloneRows(const std::list<AffineFunction>& src);

	IterationVector iterVec; 
	std::list<AffineFunction> funcs;
};

} // end poly namespace
} // end analysis namespace
} // end insieme namespace 




