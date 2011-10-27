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

#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace poly {


//==== IterationDomain ==============================================================================

IterationDomain operator&&(const IterationDomain& lhs, const IterationDomain& rhs) {
	assert(lhs.getIterationVector() == rhs.getIterationVector());
	if(lhs.isUniverse()) return rhs;
	if(rhs.isUniverse()) return lhs;

	return IterationDomain( lhs.getConstraint() and rhs.getConstraint() ); 
}

IterationDomain operator||(const IterationDomain& lhs, const IterationDomain& rhs) {
	assert(lhs.getIterationVector() == rhs.getIterationVector());
	if(lhs.isUniverse()) return rhs;
	if(rhs.isUniverse()) return lhs;

	return IterationDomain( lhs.getConstraint() or rhs.getConstraint() ); 
}

IterationDomain operator!(const IterationDomain& other) {
	return IterationDomain( not_( other.getConstraint() ) ); 
}

std::ostream& IterationDomain::printTo(std::ostream& out) const { 
	if (isEmpty()) return out << "{}";
	if (isUniverse()) return out << "{ universe }";
	return out << *constraint; 
}

//==== ScatteringFunction ==============================================================================

std::ostream& AffineSystem::printTo(std::ostream& out) const {
	out << "{" << std::endl;
	std::for_each(funcs.begin(), funcs.end(), [&](const AffineFunction& cur) { 
			out << "\t" << cur.toStr(AffineFunction::PRINT_ZEROS) <<  std::endl; 
		} ); 
	return out << "}" << std::endl;
}

void AffineSystem::insert(const AffineList::iterator& pos, const AffineFunction& af) { 
	assert( iterVec == af.getIterationVector() && 
			"Adding an affine function to a scattering matrix with a different base");

	// adding a row to this matrix 
	funcs.insert( pos, af.toBase(iterVec) );
}

void AffineSystem::cloneRows(const AffineList& src) { 
	std::for_each(src.begin(), src.end(), 
			[&] (const AffineFunction& cur) { funcs.push_back( cur.toBase(iterVec) ); } 
		);
}

} // end poly namesapce 
} // end analysis namespace 
} // end insieme namespace 

