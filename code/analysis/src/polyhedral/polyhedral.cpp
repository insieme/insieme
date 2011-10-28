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

#include <iomanip>

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace poly {

using namespace insieme::core;

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

//==== AffineSystem ==============================================================================

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

//==== Stmt ==================================================================================

std::ostream& Stmt::printTo(std::ostream& out) const {

	out << "@ S" << id << ": " << std::endl 
		<< " -> " << printer::PrettyPrinter( addr.getAddressedNode() ) << std::endl;
	
	// Print the iteration domain for this statement 

	out << " -> ID " << dom << std::endl; 

	// TupleName tn(cur.addr, "S"+utils::numeric_cast<std::string>( id ));
	// auto&& ids = makeSet<POLY_BACKEND>(ctx, dom, tn);	
	// out << " => ISL: " << ids;
	// out << std::endl;

	// Prints the Scheduling for this statement 
	out << " -> Schedule: " << std::endl << schedule;

	// auto&& scattering = makeMap<POLY_BACKEND>(ctx, *static_pointer_cast<AffineSystem>(sf), tn);
	// out << " => ISL: ";
	// scattering->printTo(out);
	// out << std::endl;
	
	// Prints the list of accesses for this statement 
	for_each(access_begin(), access_end(), [&](const poly::AccessInfo& cur){ out << cur; });
	return out;
}

//==== AccessInfo ==============================================================================

std::ostream& AccessInfo::printTo(std::ostream& out) const {
	out << " -> REF ACCESS: [" << Ref::useTypeToStr( getUsage() ) << "] "
		<< " -> VAR: " << printer::PrettyPrinter( getExpr().getAddressedNode() ) ; 

	const AffineSystemPtr& accessInfo = getAccess();
	out << " INDEX: " << join("", accessInfo->begin(), accessInfo->end(), 
			[&](std::ostream& jout, const poly::AffineFunction& cur){ jout << "[" << cur << "]"; } );
	out << std::endl;

	if (accessInfo) {	
		out << *accessInfo;
		//auto&& access = makeMap<POLY_BACKEND>(ctx, *accessInfo, tn, 
			//TupleName(cur.getExpr(), cur.getExpr()->toString())
		//);
		//// map.intersect(ids);
		//out << " => ISL: "; 
		//access->printTo(out);
		//out << std::endl;
	}
	return out;
}

} // end poly namesapce 
} // end analysis namespace 
} // end insieme namespace 

