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

#include "insieme/utils/logging.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_builder.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/analysis/dep_graph.h"

#define MSG_WIDTH 100

namespace insieme { namespace analysis { namespace polyhedral { 

using namespace insieme::core;

//==== IterationDomain ==============================================================================

IterationDomain operator&&(const IterationDomain& lhs, const IterationDomain& rhs) {
	assert(lhs.getIterationVector() == rhs.getIterationVector());
	if(lhs.universe()) return rhs;
	if(rhs.universe()) return lhs;

	return IterationDomain( lhs.getConstraint() and rhs.getConstraint() ); 
}

IterationDomain operator||(const IterationDomain& lhs, const IterationDomain& rhs) {
	assert(lhs.getIterationVector() == rhs.getIterationVector());
	if(lhs.universe()) return rhs;
	if(rhs.universe()) return lhs;

	return IterationDomain( lhs.getConstraint() or rhs.getConstraint() ); 
}

IterationDomain operator!(const IterationDomain& other) {
	return IterationDomain( not_( other.getConstraint() ) ); 
}

std::ostream& IterationDomain::printTo(std::ostream& out) const { 
	if (empty()) return out << "{}";
	if (universe()) return out << "{ universe }";
	return out << *constraint; 
}

utils::Piecewise<insieme::core::arithmetic::Formula> 
cardinality(core::NodeManager& mgr, const IterationDomain& dom) {
	auto&& ctx = makeCtx();

	SetPtr<> set = makeSet(ctx, dom);
	return set->getCard()->toPiecewise(mgr);
}

IterationDomain makeVarRange(IterationVector& 				iterVec, 
							 const core::ExpressionPtr& 	var, 
							 const core::ExpressionPtr& 	lb, 
							 const core::ExpressionPtr& 	ub) 
{
	core::ExpressionPtr expr = var;
	// check whether the lb and ub are affine expressions 
	if(expr->getType()->getNodeType() == core::NT_RefType) {
		expr = core::IRBuilder(var->getNodeManager()).deref(var);
	}
	assert(lb && "Lower bound not specified, argument required");
	core::arithmetic::Formula lbf = core::arithmetic::toFormula(lb);
	core::arithmetic::Formula ubf = ub ? core::arithmetic::toFormula(ub) : core::arithmetic::Formula();

	if (!ub || lbf == ubf) {
		AffineFunction af(iterVec, 0-lbf+core::arithmetic::Value(expr));
		return IterationDomain( AffineConstraint(af, utils::ConstraintType::EQ) );
	}
	// else this is a range 
	AffineFunction lbaf(iterVec, 0-lbf+core::arithmetic::Value(expr));
	AffineFunction ubaf(iterVec, 0-ubf+core::arithmetic::Value(expr));
	return IterationDomain( 
		AffineConstraint(lbaf, utils::ConstraintType::GE) and 
		AffineConstraint(ubaf, utils::ConstraintType::LT) 
	);
}


IterationDomain getVariableDomain(IterationVector& vec, const core::ExpressionAddress& addr) {
	
	// Find the enclosing SCoP (if any)
	core::NodeAddress prev = addr;
	core::NodeAddress parent;
	// get the immediate SCoP
	while(!prev.isRoot() && (parent = prev.getParentAddress(1)) && !parent->hasAnnotation( scop::ScopRegion::KEY) ) { prev=parent; } 

	// This statement is not part of a SCoP (also may throw an exception)
	if ( !parent->hasAnnotation( scop::ScopRegion::KEY ) ) { return IterationDomain( vec ); }

	StatementAddress enclosingScop = parent.as<StatementAddress>();

	prev = parent;
	// Iterate throgh the stateemnts until we find the entry point of the SCoP
	while(!prev.isRoot() && (parent = prev.getParentAddress(1)) && parent->hasAnnotation( scop::ScopRegion::KEY) ) { 
		prev=parent;
	} 

	assert(parent && "Scop entry not found");

	// Resolve the SCoP from the entry point
	Scop scop = prev->getAnnotation( scop::ScopRegion::KEY )->getScop();

	// navigate throgh the statements of the SCoP until we find the one 
	auto fit = std::find_if(scop.begin(), scop.end(), [&](const StmtPtr& cur) { 
			return isChildOf(cur->getAddr(),addr);
		}); 

	if (fit != scop.end()) {
		vec = scop.getIterationVector();
		// found stmt containing the requested expression 
		return IterationDomain(vec, (*fit)->getDomain());
	}

	vec = scop.getIterationVector();
	IterationDomain cur = IterationDomain(vec, enclosingScop->getAnnotation( scop::ScopRegion::KEY )->getDomainConstraints());
	// otherwise the expression is part of a condition expression 
	prev = enclosingScop;
	// Iterate throgh the stateemnts until we find the entry point of the SCoP
	while(!prev.isRoot() && (parent = prev.getParentAddress(1)) && parent->hasAnnotation( scop::ScopRegion::KEY) ) { 
		prev=parent;
		cur &= IterationDomain( vec, prev->getAnnotation( scop::ScopRegion::KEY )->getDomainConstraints() );
	} 
	return cur;
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
	// adding a row to this matrix 
	funcs.insert( pos.get(), AffineFunctionPtr(new AffineFunction(af.toBase(iterVec))) );
}

utils::Matrix<int> extractFrom(const AffineSystem& sys) {

	utils::Matrix<int> mat(sys.size(), sys.getIterationVector().size());

	size_t i=0;
	for_each (sys.begin(), sys.end(), [&](const AffineFunction& cur) {
			size_t j=0;
			std::for_each(cur.begin(), cur.end(), [&] (const AffineFunction::Term& term) {
				mat[i][j++] = term.second;
			});
			i++;
		} );

	return mat;
}

std::vector<core::VariablePtr> getOrderedIteratorsFor(const AffineSystem& sched) {
	// For each dimension we need to check whether this entry contains an iterator
	const IterationVector& iterVec = sched.getIterationVector();
	utils::Matrix<int> schedule = extractFrom(sched);

	std::vector<core::VariablePtr> iters;
	for(size_t r = 0, rend = schedule.rows(); r<rend; ++r) {
		for (size_t c = 0, cend = iterVec.getIteratorNum(); c != cend; ++c) {
			if (schedule[r][c] == 1) { 
				iters.push_back( static_cast<const Iterator&>(iterVec[c]).getExpr().as<core::VariablePtr>() ); 
				break;
			}
		}
	}
	return iters;	
}

//==== Stmt ==================================================================================

std::vector<core::VariablePtr> Stmt::loopNest() const {
	
	std::vector<core::VariablePtr> nest;
	for_each(getSchedule(),	[&](const AffineFunction& cur) { 
		const IterationVector& iv = cur.getIterationVector();
		for(IterationVector::iter_iterator it = iv.iter_begin(), end = iv.iter_end(); it != end; ++it) {
			if( cur.getCoeff(*it) != 0) { 
				nest.push_back( core::static_pointer_cast<const core::Variable>( it->getExpr()) );
				break;
			}
		}
	} );

	return nest;
}

std::ostream& Stmt::printTo(std::ostream& out) const {

	out << "@ S" << id << ": " << std::endl 
		<< " -> " << printer::PrettyPrinter( addr.getAddressedNode() ) << std::endl;
	
	// Print the iteration domain for this statement 
	out << " -> ID " << dom << std::endl; 

	// Prints the Scheduling for this statement 
	out << " -> Schedule: " << std::endl << schedule;

	// Prints the list of accesses for this statement 
	for_each(access_begin(), access_end(), [&](const AccessInfoPtr& cur){ out << *cur; });

	auto&& ctx = makeCtx();
	out << "Card: " << *makeSet(ctx, dom)->getCard() << std::endl;

	return out;
}

boost::optional<const Stmt&> getPolyheadralStmt(const core::StatementAddress& stmt) {

	NodePtr root = stmt.getRootNode();
	scop::AddressList&& addrs = scop::mark( root );

	// we have to fing whether the top level of this scop contains stmt
	auto fit = find_if(addrs.begin(), addrs.end(), [&](const NodeAddress& cur) { 
			if ( core::isChildOf(cur, core::static_address_cast<const Node>(stmt)) ) { 
				return true; 
			}
			return false;
		});
	
	if (fit == addrs.end()) {
		// the address is not inside any of the top level scops for this region
		return boost::optional<const Stmt&>();
	}
	
	assert( fit->getAddressedNode()->hasAnnotation(scop::ScopRegion::KEY) );
	scop::ScopRegion& reg = *fit->getAddressedNode()->getAnnotation(scop::ScopRegion::KEY);

	// find the statement inside this region 
	Scop& scop = reg.getScop();

	auto fit2 = find_if(scop.begin(), scop.end(), 
			[&](const StmtPtr& cur) { return cur->getAddr() == stmt; });

	assert(fit2 != scop.end());

	return boost::optional<const Stmt&>(**fit2);
}

unsigned Stmt::getSubRangeNum() const {
	bool ranges = 0;
	for_each(access_begin(), access_end(), [&] (const AccessInfoPtr& cur) { 
			if (!cur->getDomain().universe()) { ++ranges; }
		});
	return ranges;
}

//==== AccessInfo ==============================================================================

std::ostream& AccessInfo::printTo(std::ostream& out) const {
	out << " -> REF ACCESS: [" << Ref::useTypeToStr( getUsage() ) << "] "
		<< " -> VAR: " << printer::PrettyPrinter( getExpr().getAddressedNode() ) ; 

	const AffineSystem& accessInfo = getAccess();
	out << " INDEX: " << join("", accessInfo.begin(), accessInfo.end(), 
			[&](std::ostream& jout, const AffineFunction& cur){ jout << "[" << cur << "]"; } );
	
	if (hasDomainInfo()) 
		out << " RANGE: " << getDomain();
	
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

std::ostream& Scop::printTo(std::ostream& out) const {
	out << std::endl << std::setfill('=') << std::setw(MSG_WIDTH) << std::left << "@ SCoP PRINT";	
	// auto&& ctx = BackendTraits<POLY_BACKEND>::ctx_type();
	out << "\nNumber of sub-statements: " << size() << std::endl;
		
	out << "IV: " << getIterationVector() << std::endl;
	for_each(begin(), end(), [&](const StmtPtr& cur) {
		out << std::setfill('~') << std::setw(MSG_WIDTH) << "" << std::endl << *cur << std::endl; 
	} );

	return out << std::endl << std::setfill('=') << std::setw(MSG_WIDTH) << "";
}

// Adds a stmt to this scop. 
void Scop::push_back( const Stmt& stmt ) {
	
	AccessList access;
	for_each(stmt.access_begin(), stmt.access_end(), 
			[&] (const AccessInfoPtr& cur) { 
				access.push_back( std::make_shared<AccessInfo>( iterVec, *cur ) ); 
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
		[&](const StmtPtr& scopStmt) { 
			size_t cur_loopnest=scopStmt->loopNest().size();
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
MapPtr<> createScatteringMap(CtxPtr<>&    					ctx, 
									const IterationVector&	iterVec,
									SetPtr<>& 				outer_domain, 
									const Stmt& 				cur, 
									TupleName						tn,
									size_t 							scat_size)
{
	
	auto&& domainSet = makeSet(ctx, cur.getDomain(), tn);
	assert( domainSet && "Invalid domain" );

	// Also the accesses can define restriction on the domain (e.g. MPI calls)
	std::for_each(cur.access_begin(), cur.access_end(), [&](const AccessInfoPtr& cur){
			domainSet *= makeSet(ctx, cur->getDomain(), tn);
		});
	outer_domain = outer_domain + domainSet;
	
	AffineSystem sf = cur.getSchedule();

	// Because the scheduling of every statement has to have the same number of elements
	// (same dimensions) we append zeros until the size of the affine system is equal to 
	// the number of dimensions used inside this SCoP for the scheduling functions 
	for ( size_t s = sf.size(); s < scat_size; ++s ) {
		sf.append( AffineFunction(iterVec) );
	}

	return makeMap(ctx, sf, tn);
}

void buildScheduling(
		CtxPtr<>& 						ctx, 
		const IterationVector& 			iterVec,
		SetPtr<>& 						domain,
		MapPtr<>& 						schedule,
		MapPtr<>& 						reads,
		MapPtr<>& 						writes,
		const Scop::const_iterator& 	begin, 
		const Scop::const_iterator& 	end,
		size_t							schedDim)
		
{
	std::for_each(begin, end, [ & ] (const StmtPtr& cur) { 
		// Creates a name mapping which maps an entity of the IR (StmtAddress) 
		// to a name utilied by the framework as a placeholder 
		TupleName tn(cur, "S" + utils::numeric_cast<std::string>(cur->getId()));

		schedule = schedule + createScatteringMap(ctx, iterVec, domain, *cur, tn, schedDim);

		// Access Functions 
		std::for_each(cur->access_begin(), cur->access_end(), [&](const AccessInfoPtr& cur){
			const AffineSystem& accessInfo = cur->getAccess();

			if (accessInfo.empty())  return;

			auto&& access = 
				makeMap(ctx, accessInfo, tn, TupleName(cur->getExpr(), cur->getExpr()->toString()));

			// Get the domain for this statement
			SetPtr<> dom = makeSet(ctx, cur->getDomain(), tn);
			switch ( cur->getUsage() ) {
			// Uses are added to the set of read operations in this SCoP
			case Ref::USE: 		reads += access * dom;
								break;
			// Definitions are added to the set of writes for this SCoP
			case Ref::DEF: 		writes += access * dom;
								break;
			// Undefined accesses are added as Read and Write operations 
			case Ref::UNKNOWN:	reads  += access * dom;
								writes += access * dom;
								break;
			default:
				assert( false && "Usage kind not defined!" );
			}
		});
	});
}

} // end anonymous namespace

core::NodePtr Scop::toIR(core::NodeManager& mgr, const CloogOpts& opts) const {

	auto&& ctx = makeCtx();

	// universe set 
	auto&& domain 	= makeSet(ctx, IterationDomain(iterVec, true));
	auto&& schedule = makeEmptyMap(ctx, iterVec);
	auto&& reads    = makeEmptyMap(ctx, iterVec);
	auto&& writes   = makeEmptyMap(ctx, iterVec);

	buildScheduling(ctx, iterVec, domain, schedule, reads, writes, begin(), end(), schedDim());

	return polyhedral::toIR(mgr, iterVec, ctx, domain, schedule, opts);
}

MapPtr<> Scop::getSchedule(CtxPtr<>& ctx) const {
	auto&& domain 	= makeSet(ctx, IterationDomain(iterVec, true));
	auto&& schedule = makeEmptyMap(ctx, iterVec);
	auto&& empty    = makeEmptyMap(ctx, iterVec);

	buildScheduling(ctx, iterVec, domain, schedule, empty, empty, begin(), end(), schedDim());
	return schedule;
}

SetPtr<> Scop::getDomain(CtxPtr<>& ctx) const {
	auto&& domain 	= makeSet(ctx, IterationDomain(iterVec, true));
	auto&& schedule = makeEmptyMap(ctx, iterVec);
	auto&& empty    = makeEmptyMap(ctx, iterVec);

	buildScheduling(ctx, iterVec, domain, schedule, empty, empty, begin(), end(), schedDim());
	return domain;
}

MapPtr<> Scop::computeDeps(CtxPtr<>& ctx, const unsigned& type) const {
	// universe set 
	auto&& domain   = makeSet(ctx, IterationDomain(iterVec, true));
	auto&& schedule = makeEmptyMap(ctx, iterVec);
	auto&& reads    = makeEmptyMap(ctx, iterVec);
	auto&& writes   = makeEmptyMap(ctx, iterVec);
	// for now we don't handle may dependencies, therefore we use an empty map
	auto&& may		= makeEmptyMap(ctx, iterVec);

	buildScheduling(ctx, iterVec, domain, schedule, reads, writes, begin(), end(), schedDim());

	// We only deal with must dependencies for now : FIXME
	auto&& mustDeps = makeEmptyMap(ctx, iterVec);

	if ((type & dep::RAW) == dep::RAW) {
		auto&& rawDep = buildDependencies( ctx, domain, schedule, reads, writes, may ).mustDep;
		mustDeps = rawDep;
	}
	
	if ((type & dep::WAR) == dep::WAR) {
		auto&& warDep = buildDependencies( ctx, domain, schedule, writes, reads, may ).mustDep;
		mustDeps += warDep;
	}

	if ((type & dep::WAW) == dep::WAW) {
		auto&& wawDep = buildDependencies( ctx, domain, schedule, writes, writes, may ).mustDep;
		mustDeps += wawDep;
	}

	if ((type & dep::RAR) == dep::RAR) {
		auto&& rarDep = buildDependencies( ctx, domain, schedule, reads, reads, may ).mustDep;
		mustDeps += rarDep;
	}
	return mustDeps;
}

#include <isl/schedule.h>

core::NodePtr Scop::optimizeSchedule( core::NodeManager& mgr ) {
	auto&& ctx = makeCtx();

	auto&& domain   = makeSet(ctx, IterationDomain(iterVec, true));
	auto&& schedule = makeEmptyMap(ctx, iterVec);
	auto&& empty    = makeEmptyMap(ctx, iterVec);

	buildScheduling(ctx, iterVec, domain, schedule, empty, empty, begin(), end(), schedDim());
	
	auto&& depsKeep = computeDeps(ctx, dep::RAW | dep::WAR | dep::WAW);
	auto&& depsMin  =	computeDeps(ctx, dep::ALL);
	
	isl_schedule* isl_sched = 
		isl_union_set_compute_schedule(domain->getIslObj(), depsKeep->getIslObj(), depsMin->getIslObj());

	isl_union_map* umap = isl_schedule_get_map(isl_sched);
	isl_schedule_free(isl_sched);

	MapPtr<> map(*ctx, umap);
	
	return polyhedral::toIR(mgr, iterVec, ctx, domain, map);
}

bool Scop::isParallel(core::NodeManager& mgr) const {

	dep::DependenceGraph&& depGraph = 
		dep::extractDependenceGraph(mgr, *this, dep::RAW | dep::WAR | dep::WAW);

	dep::DependenceList depList = depGraph.getDependencies();
	bool parallelizable = true;
	for_each(depList, [&](const dep::DependenceInstance& cur) {
			if( std::get<3>(cur).first.size() > 1 && std::get<3>(cur).first[0] != 0 ) {
				// we have a loop carried dep. in the first dimension, this ScoP is not parallelizable
				parallelizable = false;
			}
		});

	return parallelizable;
}

} } } // end insimee::ananlsyis::polyhedral namesapce 

