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
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

namespace insieme {
namespace analysis {
namespace poly {

using namespace insieme::core;
using namespace insieme::analysis::poly;

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
	std::for_each(funcs.begin(), funcs.end(), [&](const AffineFunctionPtr& cur) { 
			out << "\t" << cur->toStr(AffineFunction::PRINT_ZEROS) <<  std::endl; 
		} ); 
	return out << "}" << std::endl;
}

void AffineSystem::insert(const iterator& pos, const AffineFunction& af) { 
	assert( iterVec == af.getIterationVector() && 
			"Adding an affine function to a scattering matrix with a different base");

	// adding a row to this matrix 
	funcs.insert( pos.get(), AffineFunctionPtr(new AffineFunction(af.toBase(iterVec))) );
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

	const AffineSystem& accessInfo = getAccess();
	out << " INDEX: " << join("", accessInfo.begin(), accessInfo.end(), 
			[&](std::ostream& jout, const poly::AffineFunction& cur){ jout << "[" << cur << "]"; } );
	out << std::endl;

	if (!accessInfo.empty()) {	
		out << accessInfo;
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

//==== Scop ====================================================================================

// Adds a stmt to this scop. 
void Scop::push_back( const Stmt& stmt ) {
	
	AccessList access;
	for_each(stmt.access_begin(), stmt.access_end(), 
			[&] (const AccessInfo& cur) { 
				access.push_back( AccessInfo( iterVec, cur ) ); 
			}
		);

	stmts.push_back( std::make_shared<Stmt>(
				stmt.getId(), 
				stmt.getAddr(), 
				IterationDomain(iterVec, stmt.getDomain()),
				AffineSystem(iterVec, stmt.getSchedule()), 
				access
			) 
		);
	size_t dim = stmts.back()->getSchedule().size();
	if (dim > sched_dim) {
		sched_dim = dim;
	}
}

// This function determines the maximum number of loop nests within this region 
// The analysis should be improved in a way that also the loopnest size is weighted with the number
// of statements present at each loop level.
size_t Scop::nestingLevel() const {
	size_t max_loopnest=0;
	for_each(begin(), end(), 
		[&](const poly::StmtPtr& scopStmt) { 
			size_t cur_loopnest=0;
			for_each(scopStmt->getSchedule(), 
				[&](const AffineFunction& cur) { 
					for(auto&& it=cur.begin(), end=cur.end(); it!=end; ++it) {
						if((*it).second != 0 && (*it).first.getType() == Element::ITER) { 
							++cur_loopnest; 
							break;
						}
					}
				} );
			if (cur_loopnest > max_loopnest) {
				max_loopnest = cur_loopnest;
			}
		} );
	return max_loopnest;
}

namespace {

// Creates the scattering map for a statement inside the SCoP. This is done by building the domain
// for such statement (adding it to the outer domain). Then the scattering map which maps this
// statement to a logical execution date is transformed into a corresponding Map 
poly::MapPtr<BackendTraits<POLY_BACKEND>::ctx_type> 
createScatteringMap(
		BackendTraits<POLY_BACKEND>::ctx_type& 					ctx, 
		const poly::IterationVector&							iterVec,
		poly::SetPtr<BackendTraits<POLY_BACKEND>::ctx_type>& 	outer_domain, 
		const poly::Stmt& 										cur, 
		size_t 													scat_size ) 
{
	// Creates a name mapping which maps an entity of the IR (StmtAddress) 
	// to a name utilied by the framework as a placeholder 
	TupleName tn(cur.getAddr(), "S" + utils::numeric_cast<std::string>(cur.getId()));

	auto&& domainSet = makeSet<POLY_BACKEND>(ctx, cur.getDomain(), tn);
	assert( domainSet && "Invalid domain" );
	outer_domain = set_union(ctx, *outer_domain, *domainSet);

	AffineSystem sf = cur.getSchedule();
	// Because the scheduling of every statement has to have the same number of elements
	// (same dimensions) we append zeros until the size of the affine system is equal to 
	// the number of dimensions used inside this SCoP for the scheduling functions 
	for ( size_t s = sf.size(); s < scat_size; ++s ) {
		sf.append( AffineFunction(iterVec) );
	}

	return makeMap<POLY_BACKEND>(ctx, sf, tn);
}

} // end anonymous namespace

core::NodePtr Scop::toIR(core::NodeManager& mgr) const {
	auto&& ctx = BackendTraits<POLY_BACKEND>::ctx_type();

	// universe set 
	auto&& domain = makeSet<POLY_BACKEND>(ctx, IterationDomain(iterVec));
	auto&& schedule = makeEmptyMap<POLY_BACKEND>(ctx, iterVec);
	
	std::for_each(begin(), end(), 
		[ & ] (const poly::StmtPtr& cur) { 
			schedule = map_union( ctx, *schedule, *createScatteringMap(ctx, iterVec, domain, *cur, schedDim()) );
		}
	);

	return poly::toIR(mgr, iterVec, ctx, *domain, *schedule);
}

} // end poly namesapce 
} // end analysis namespace 
} // end insieme namespace 

