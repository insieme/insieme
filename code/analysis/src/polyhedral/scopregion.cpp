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

#include <stack>

#include "insieme/analysis/func_sema.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/scopregion.h"
#include "insieme/analysis/polyhedral/scopvisitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/set_utils.h"

#define AS_EXPR_ADDR(addr) static_address_cast<const Expression>(addr)

namespace insieme { namespace analysis { namespace polyhedral {

using namespace insieme::core;
using namespace insieme::core::lang;

using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;
using namespace insieme::analysis::polyhedral::scop;

using arithmetic::Piecewise;
using arithmetic::Formula;

// FIXME: can we remove that forward decl?
void postProcessSCoP(const NodeAddress& scop, AddressList& scopList);

/// Determines the maximum node path depth given node n as the root node. This procedure is most likely be
/// used before or during a PrintNodes invocation.
unsigned int maxDepth(const insieme::core::Address<const insieme::core::Node> n) {
	unsigned max = 0;
	for (auto c: n.getChildAddresses()) {
		unsigned now=maxDepth(c);
		if (now>max) max=now;
	}
	return max+1;
}

/// Print the nodes to std::cout starting from root n, one by one, displaying the node path
/// and the visual representation. The output can be prefixed by a string, and for visually unifying several
/// PrintNodes invocations the maximum (see MaxDepth) and current depth may also be given.
void printNodes(const insieme::core::Address<const insieme::core::Node> n,
								  std::string prefix="        @\t", int max=-1, unsigned int depth=0) {
	if (max<0) max=maxDepth(n);
	int depth0=n.getDepth()-depth;
	std::cout << prefix << n << std::setw(2*(max-depth+1)) << "+"
			  << std::setw(1+2*(depth0+depth-1)) << "" << *n << std::endl;
	for (auto c: n.getChildAddresses())
		if (depth<(unsigned int)max) printNodes(c, prefix, max, depth+1);
}

// FIXME: can we remove that namespace usage?
using namespace insieme::utils;

/** After the outermost for-loop has been found in a SCoP (using the bottom-up technique) we start the top-down pass to
    detect conditions which would invalidate the SCoP. For example, an assignment to parameters of the iteration vector
    makes a SCoP invalid. In order to detect this, we have to consider the top-level iteration vector and visit all the
    statements inside the SCoP looking for assignment statements where the left side is one of the parameters of iteration
    vector.
    Is called from postProcessSCoP, as well as recursively.
*/
void detectInvalidSCoPs(const IterationVector& iterVec, const NodeAddress& scop) {
	assert ( scop->hasAnnotation(ScopRegion::KEY) );

	ScopRegion& region = *scop->getAnnotation( ScopRegion::KEY );
	const StmtVect& stmts = region.getDirectRegionStmts();

	std::for_each(stmts.begin(), stmts.end(), [&](const scop::Stmt& curStmt) {
		
		const scop::Stmt::RefAccessList& ail = curStmt.getRefAccesses();

		std::for_each(ail.begin(), ail.end(), [&] (const ReferencePtr& cur) {

				// if( usage != Ref::SCALAR && usage != Ref::MEMBER) { continue; }

				ExpressionPtr addr = cur->refExpr;
				switch ( cur->usage ) {
				case Ref::DEF:
				case Ref::UNKNOWN:
				{
					if ( iterVec.getIdx( addr ) != -1 ) {
						// This SCoP has to be discarded because one of the iterators or parameters
						// of the iteration domain has been overwritten within the body of the SCoP
						THROW_EXCEPTION(DiscardSCoPException, "Assignment to element of the iteration vector detected",  
							curStmt.getAddr().getAddressedNode(), addr 
						);
					}
				}
				default:
					break;
				}
			});

		// otherwise if one of the parameters of the SCoP is being defined in the body of the SCoP,
		// then this region must be invalided as well
		if (curStmt.getAddr()->getNodeType() == NT_DeclarationStmt) {
			
			// std::cout << iterVec << std::endl;
			// std::cout << *curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode() << std::endl;

			// make sure the declared variable is not one of the parameters of the SCoP
			if ( iterVec.contains( curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode() ) ) {
				THROW_EXCEPTION(DiscardSCoPException, "Declaration for one of the parameters of the SCoP is within the SCoP",  
						curStmt.getAddr().getAddressedNode(), curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode()
					);
			}
		}

		});

	// now check stmts of the subScops
	const SubScopList& subScops = region.getSubScops();
	std::for_each(subScops.begin(), subScops.end(), [&](const SubScop& cur) { 
		detectInvalidSCoPs(iterVec, cur.first); 
	} );
}

/** postProcessSCoP makes sure that within a SCoP no parameter value is modified and that iterators are only modified
    within loop statements.
    @param scop NodeAddress representing the SCoP that will be tested
    @scopList vector<NodeAddress> representing the SCoPs that are valid (if everything goes well, this list has 1 elem which is the input parameter "scop")

    TODO: postProcessSCoP should only look at the current SCoP annotation only, not looking at child nodes. Child SCoPs
    should be handled independently in some other (to-be-written wrapper) code. There should not be a scopList, merging
    valid SCoPs together should also be done centrally, instead of recursively. So, postProcessSCop should return
    a boolean value: true, if the SCoP can be handled, false otherwise (same as region.invalid).
*/
void postProcessSCoP(const NodeAddress& scop, AddressList& scopList) {
	// first, make sure that the node has been marked as SCoP
	if (!scop->hasAnnotation(ScopRegion::KEY)) return;
	ScopRegion& region = *scop->getAnnotation( ScopRegion::KEY );
	if (!region.valid) return;

	const IterationVector& iterVec = region.getIterationVector();

	try {
		detectInvalidSCoPs(iterVec, scop); // this will throw an exception if the SCoP is deemed invalid
		// add the current SCoP (which has been confirmed valid) to the list of valid SCoPs
		scopList.push_back(scop);          // this will execute only if the SCoP is deemed valid
	} catch(const DiscardSCoPException& e ) {
		LOG(WARNING) << "Invalidating SCoP because iterator/parameter '" << 
				*e.expression() << "' is being assigned in stmt: '" << *e.node() << "'";

		// recursively call ourselves: our SCoP is invalid, but perhaps one of our nested for-loop is a proper SCoP
		std::for_each(region.getSubScops().begin(), region.getSubScops().end(), 
				[&](const SubScop& subScop) { postProcessSCoP(subScop.first, scopList); });

		// Invalidate the annotation for this SCoP, we can set the valid flag to false because we
		// are sure that within this SCoP there are issues with makes the SCoP not valid. 
		region.valid=false;
	}
}

namespace scop {

using namespace core;

//===== ScopRegion =================================================================================
const string ScopRegion::NAME = "SCoPAnnotation";
const utils::StringKey<ScopRegion> ScopRegion::KEY("SCoPAnnotation");

std::ostream& ScopRegion::printTo(std::ostream& out) const {
	out << "IterVec: " << iterVec;
	out << "\\nIterDom: ";
	if (domain.empty()) 	out << "{ }";
	else					out << domain;
	
	out << "\\n# of direct stmts: " << stmts.size();
	out << "\\nSub SCoPs: {";
	std::for_each(subScops.begin(), subScops.end(),
		[&](const SubScop& cur) { out << cur.second << ", "; });
	return out << "}";
}

bool ScopRegion::containsLoopNest() const {
	return iterVec.getIteratorNum() > 0;
}

boost::optional<Scop> ScopRegion::toScop(const core::NodePtr& root) {
	AddressList&& al = insieme::analysis::polyhedral::scop::mark(root);
	if(al.empty() || al.size() > 1 || al.front().getDepth() > 1) {
		// If there are not scops or the number of scops is greater than 2
		// or the the extracted scop is not the top level node
		return boost::optional<Scop>();
	}
	assert(root->hasAnnotation(ScopRegion::KEY));
	ScopRegion& ann = *root->getAnnotation(ScopRegion::KEY);

	ann.resolve();

	if (!ann.valid) return boost::optional<Scop>();
	return boost::optional<Scop>(ann.getScop());
}

/** Recursively process ScopRegions caching the information related to access functions and
scattering matrices for the statements contained in this Scop region */
void resolveScop(const IterationVector& 		iterVec, 
				 IterationDomain			 	parentDomain, 
	   		   	 const ScopRegion& 				region,
				 size_t&						pos,
				 size_t&						id,
 				 const AffineSystem&	 		curScat,
				 ScopRegion::IteratorOrder&		iterators,
				 Scop& 							scat) 
{
	typedef std::set<Iterator> IteratorSet;
	// assert( parentDomain->getIterationVector() == iterVec );
	IterationDomain currDomain = parentDomain && IterationDomain(iterVec, region.getDomainConstraints());
	const StmtVect& scopStmts = region.getDirectRegionStmts();
	
	// for every access in this region, convert the affine constraint to the new iteration vector 
	std::for_each(scopStmts.begin(), scopStmts.end(), [&] (const Stmt& cur) {
			
		StatementPtr&& curPtr = cur.getAddr().getAddressedNode();
		assert(curPtr->getNodeType() != core::NT_MarkerExpr && curPtr->getNodeType() != core::NT_MarkerStmt);
	
		IterationDomain thisDomain = currDomain;

		AffineSystem newScat(curScat);
		const IterationVector& iterVec = curScat.getIterationVector();
		assert(&newScat.getIterationVector() == &iterVec); 
		AffineFunction af( iterVec );

		// check wheather the statement is a SCoP
		auto fit = std::find_if(region.getSubScops().begin(), region.getSubScops().end(), 
			[&](const SubScop& subScop) -> bool { 
				return subScop.first.getAddressedNode() == cur.getAddr().getAddressedNode(); 
			} 
		);

		if (fit != region.getSubScops().end() ) {
			// add the IterationDomain stored in the pointer to the current domain and recursively
			// resolve the ScopRegion 
			thisDomain &= IterationDomain(iterVec, fit->second);

			if(curPtr->getNodeType() != NT_ForStmt) {
				assert(curPtr->hasAnnotation(ScopRegion::KEY));
				resolveScop( iterVec, 
							 thisDomain, 
							 *curPtr->getAnnotation(ScopRegion::KEY), 
							 pos, 
							 id,
							 curScat, 
							 iterators, 
							 scat
						   );
				return;
			}
		}

		af.setCoeff(Constant(), pos++);
		newScat.append( af );

		// this is a sub scop
		if (curPtr->hasAnnotation(ScopRegion::KEY)) {

			if ( curPtr->getNodeType() == NT_ForStmt ) {
				// if the statement is a loop, then we append a dimension with the corresponding
				// iterator variable and we go recursively to visit the body  
				const ForStmtPtr& forStmt = curPtr.as<ForStmtPtr>();
				const VariablePtr& iter = forStmt->getIterator();

				AffineFunction newAf( iterVec );
				newAf.setCoeff( Iterator(iter), 1 );
				newScat.append(newAf); 
				
				iterators.push_back(Iterator(iter));
			} 

			size_t nestedPos = 0;
			resolveScop( iterVec, 
						 thisDomain, 
						 *curPtr->getAnnotation(ScopRegion::KEY), 
						 nestedPos, 
						 id,
						 newScat, 
						 iterators, 
						 scat
					   );

			// pop back the iterator in the case the statement was a for stmt
			if ( curPtr->getNodeType() == NT_ForStmt ) { iterators.pop_back(); }
			return;
		}

		// Because some statements may introduce fake iterators we have to be sure to not nullify that value afterwards
		// This array will store all the fake iterators introduced by the current statement 
		std::set<VariablePtr> fakeIterators;

		// Access expressions 
		const Stmt::RefAccessList& refs = cur.getRefAccesses();
		std::vector<AccessInfoPtr> accInfo;
		std::for_each(refs.begin(), refs.end(), [&] (const ReferencePtr& curRef) {
				AffineSystem idx(iterVec);

				std::shared_ptr<IterationDomain> domain = std::make_shared<IterationDomain>(iterVec);

				switch (curRef->type) {
				case Ref::SCALAR:
				case Ref::MEMBER:
					// A scalar is treated as a zero dimensional array 
					idx.append( AffineFunction(iterVec) );
					break;
				case Ref::ARRAY:
				{
					assert ((!curRef->indecesExpr.empty() || curRef->range) && "Array access without index specifier");

					if (curRef->range) {
						// This is a range access
						assert(curRef->indecesExpr.size() == 1);
						domain = std::make_shared<IterationDomain>( cloneConstraint(iterVec, curRef->range) );	

						// This statements introduced an iterator, therefore we have to make sure we do not set it zero
						// afterwards
						fakeIterators.insert( curRef->indecesExpr.front().as<VariablePtr>() );
						
						// create the affine function 1*fakeIter
						AffineFunction af(iterVec);
						af.setCoeff(curRef->indecesExpr.front().as<VariablePtr>(), 1);
						idx.append( af );
						break;
					}

					for_each(curRef->indecesExpr, [&](const ExpressionPtr& cur) { 
							assert(cur->hasAnnotation(scop::AccessFunction::KEY));
							scop::AccessFunction& ann = *cur->getAnnotation(scop::AccessFunction::KEY);
							idx.append( ann.getAccessFunction().toBase(iterVec) );
						}
					);
					break;
				}
				default:
					VLOG(1) << "Reference of type " << Ref::refTypeToStr(curRef->type) << " not handled!";
				}

				assert(domain);

				accInfo.push_back( 
					std::make_shared<AccessInfo>( 
							AS_EXPR_ADDR( concat<Node>(cur.getAddr(), curRef->refExpr ) ), 
							curRef->type, 
							curRef->usage, 
							idx,
							*domain
						)
					);
		});

		// save the domain 
		AffineConstraintPtr saveDomain = currDomain.getConstraint();

		IteratorSet nested_iters(iterators.begin(), iterators.end()), 
					domain_iters(iterVec.iter_begin(), iterVec.iter_end()), 
					used = getIterators(saveDomain),
					notUsed;
		
		std::copy(fakeIterators.begin(), fakeIterators.end(), std::inserter(nested_iters, nested_iters.begin()));
		std::copy(used.begin(), used.end(), std::inserter(nested_iters, nested_iters.begin()));

		// Remove iterators which do not belong to this nested region
		std::set_difference(
			domain_iters.begin(), domain_iters.end(), nested_iters.begin(), nested_iters.end(), 
			std::inserter(notUsed, notUsed.begin())
		);

		// set to zero all the not used iterators 
		std::for_each(notUsed.begin(), notUsed.end(), 
				[&] (const Iterator& curIt) { 
					if ( curIt.isExistential() ) { return; }

					AffineFunction af(iterVec);
					af.setCoeff(curIt, 1);
					af.setCoeff(Constant(), 0);
					saveDomain = saveDomain and AffineConstraint(af, ConstraintType::EQ);
				}
			);

		IterationDomain iterDom = saveDomain ? IterationDomain(saveDomain) : IterationDomain(iterVec);
		auto s=insieme::analysis::polyhedral::Stmt(id++, cur.getAddr(), iterDom, newScat, accInfo);
		scat.push_back(s);
	} ); 
}

namespace {

	/**
	 * This utility function is collecting all locally declared variables within the given
	 * code fragment and maps it to the list of enclosing iterator variables (in the corresponding order).
	 *
	 * @param cur the code fragment to be searched
	 * @return a map mapping all locally declared variables to the surrounding iterator variables
	 */
	std::map<core::VariablePtr, core::VariableList> collectLocalVars(const core::NodePtr& cur) {

		// collect all local declarations
		std::vector<core::DeclarationStmtAddress> decls;
		core::visitDepthFirst(core::NodeAddress(cur), [&](const core::DeclarationStmtAddress& cur) {
			// we only collect declaration statements outside for loops
			if (!cur.isRoot() && cur.getParentNode()->getNodeType() != core::NT_ForStmt) {
				decls.push_back(cur);
			}
		});

		// compute list of enclosing iterator variables
		std::map<core::VariablePtr, core::VariableList> res;
		for_each(decls, [&](const core::DeclarationStmtAddress& cur) {

			core::VariablePtr var = cur->getVariable();

			// collect iterator variables
			auto collector = core::makeLambdaVisitor([&](const ForStmtPtr& cur) {
				res[var].push_back(cur->getIterator());
			});

			core::visitPathTopDown(cur, collector);
		});

		return res;
	}
}

/** Resolve the SCoP, this means adapt all the access expressions on nested SCoPs to this level and cache all the
scattering info at this level */
void ScopRegion::resolve() const {
	assert(valid && "Error Try to resolve an invalid SCoP");

	// If the region has been already resolved, we simply return the cached result
	if ( isResolved() ) { return; }

	// we compute the full scattering information for this domain and we cache the result for
	// later use. 
	scopInfo = std::make_shared<Scop>( iterVec );

	AffineSystem sf( getIterationVector() );
	ScopRegion::IteratorOrder iterOrder;
	
	// std::cout << *annNode << std::endl;
	// in the case the entry point of this scop is a forloop, then we build the scattering matrix
	// using the loop iterator index 
	if (annNode->getNodeType() == NT_ForStmt) {
		AffineFunction af( getIterationVector() );
		sf.append( af ); // the first dimension is composed by all zeros
		Iterator iter = Iterator(core::static_pointer_cast<const ForStmt>(annNode)->getIterator());
		af.setCoeff( iter, 1 );
		sf.append( af );
		iterOrder.push_back(iter);
	}

	size_t pos=0, id=0;
	resolveScop(
			getIterationVector(), 
			IterationDomain(getIterationVector()), 
			*this, 
			pos, 
			id,
			sf, 
			iterOrder, 
			*scopInfo
		);


	// --- fix interpretation of local variables ---

	// a map linking all locally declared variables to the list of surrounding iterators
	std::map<core::VariablePtr, core::VariableList> localVar = collectLocalVars(annNode);

	// update access functions for local variables
	for_each(*scopInfo, [&](StmtPtr& stmt) {

		// search accesses
		for_each(stmt->getAccess(), [&](AccessInfoPtr& access) {

			// extract variable (if there is one)
			core::ExpressionPtr expr = access->getExpr().getAddressedNode();
			if (expr->getNodeType() != core::NT_Variable) {
				return;
			}

			core::VariablePtr var = expr.as<core::VariablePtr>();

			// check whether variable is local
			auto pos = localVar.find(var);
			if (pos == localVar.end()) {
				return;	// it is not
			}

			// update affine access function
			AffineSystem&  accessFunctions = access->getAccess();

			// re-build access functions
			accessFunctions.clear();

			for_each(pos->second, [&](const core::VariablePtr& iter) {
				vector<int> coefficients;
				for_each(iterVec, [&](const Element& cur) {
					coefficients.push_back((cur.getType() == Element::ITER && *static_cast<const Iterator&>(cur).getVariable() == *iter) ? 1 : 0);
				});
				accessFunctions.append(AffineFunction(iterVec, coefficients));
			});

		});
	});

	assert( isResolved() );
}

//===== AccessFunction ============================================================
const string AccessFunction::NAME = "AccessFuncAnn";
const utils::StringKey<AccessFunction> AccessFunction::KEY("AccessFuncAnnKey");

std::ostream& AccessFunction::printTo(std::ostream& out) const {
	return out << "IV: " << iterVec << ", Access: " << access;
}

/** mark is the main entry point for SCoP analysis: It finds and marks the SCoPs contained in the root program tree and
returns a list of found SCoPs (an empty list in the case no SCoP was found). */
AddressList mark(const core::NodePtr& root) {
	AddressList allscops, validscops;
	ScopVisitor sv(allscops);

	try {
		sv.visit(NodeAddress(root));
		if (root->hasAnnotation(ScopRegion::KEY)) allscops.push_back(NodeAddress(root));
	} catch (const NotASCoP& e) {
		LOG(WARNING) << e.what();
	}

	// remove SCoP from our list of SCoPs if the SCoP does not satisfy the needs of the polyhedral model
	// initially, the second argument "validscops" is empty and will be filled by postProcessSCoP
	// FIXME: Bug in the SCoP detection: Empty SCoPs are returned as well. We filter these before determining
	// the validity of the SCoP.
	for (const auto& scopnode: allscops) {
		//Scop& scop=Scop::getScop(scopnode);
		if (true /*scop.size()>0*/) {
			postProcessSCoP(scopnode, validscops);
		} else {
			annotations::LocationOpt loc=annotations::getLocation(scopnode);
			LOG(WARNING) << "Found SCoP " << scopnode //<< " of size " << scop.size()
						 //<< " and nestinglvl " << scop.nestingLevel()
						 << " at address " << *loc << ", ignoring" << std::endl;
		}
	}

	return validscops;
}

} } } }
