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

#include <vector>
#include <map>

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_address.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/defuse_collect.h"

#include "boost/optional.hpp"

namespace insieme {
namespace analysis {
namespace scop {

typedef std::vector<core::StatementAddress> StmtAddressList;

// Set of array accesses which appears strictly within this SCoP, array access in sub SCoPs will
// be directly referred from sub SCoPs. The accesses are ordered by the appearance in the SCoP
typedef std::vector<RefPtr> RefAccessList; 

/**************************************************************************************************
 * ScopStmt: Utility class which contains all the information of statements inside a SCoP (both
 * direct and contained in sub-scops). 
 *
 * A statement into a SCoP has 3 piece of information associated:
 * 	- Iteration domain:    which define the domain on which this statement is valid
 * 	- Scattering function: which define the order of execution with respect of other statements in
 * 						   the SCoP region
 *  - Accesses: pointers to refs (either Arrays/Scalars/Memebrs) which are defined or used within 
 *              the statement. 
 *
 * This is information is not computed when the the SCoP region is first build but instead on demand
 * (lazy) and cached for future requests. 
 *************************************************************************************************/
class ScopStmt { 
	
	core::StatementAddress 			address;
	poly::ScatteringFunctionPtr		scattering;
	RefAccessList					accesses;

public:
	ScopStmt(const core::StatementAddress& addr) : address(addr) { }

	const core::StatementAddress& getAddr() const { return address; }
	
	const core::StatementAddress& operator->() const { return address; }

	// FIXME:
	const poly::ScatteringFunction& getScattering() const { 
		if (!scattering) {
			// compute the scattering and cache the result
		}
		assert(scattering && "Scattering not computed!");
		return *scattering;
	}

	const RefAccessList& getRefAccesses() const { return accesses; }

	bool operator<(const ScopStmt& other) const { return address < other.address; }
	
	~ScopStmt() { }
};

typedef std::vector<ScopStmt> ScopStmtList;

/************************************************************************************************** 
 * ScopRegion: Stores the information related to a SCoP (Static Control Part) region of a program.
 * The IterationVector which is valid within the SCoP body and the set of constraints which define
 * the entry point for this SCoP.
 *
 * This annotation is attached to every node introducing changes to the iteration domain. Nodes
 * which carries ScopAnnotations are: ForLoops, IfStmt and LambdaExpr. 
 *
 * Each ScopAnnotation keeps a list of references to Sub SCoPs contained in this region (if present)
 * and the list of ref accesses directly present in this region. Accesses in the sub region are not
 * directly listed in the current region but retrieval is possible via the aforementioned pointer to
 * the sub scops. 
 *************************************************************************************************/
class ScopRegion: public core::NodeAnnotation {
public:
	static const string NAME;
	static const utils::StringKey<ScopRegion> KEY;

	typedef std::map<core::StatementAddress, poly::ScatteringFunction> StmtScattering;

	/********************************************************************************************** 
	 * AccessInfo is a tuple which gives the list of information associated to a ref access: i.e.
	 * the pointer to a RefPtr instance (containing the ref to the variable being accessed and the
	 * type of access (DEF or USE). The iteration domain which defines the domain on which this
	 * access is defined and the access functions for each dimensions.
	 *********************************************************************************************/
	typedef std::tuple< 
		RefPtr, poly::ConstraintCombinerPtr, std::vector<poly::ConstraintCombinerPtr>
	> AccessInfo;

	typedef std::vector<AccessInfo> AccessInfoList;

	ScopRegion( const poly::IterationVector& iv, 
			const poly::IterationDomain& comb = poly::IterationDomain(),
			const ScopStmtList& stmts = ScopStmtList(),
			const StmtAddressList& subScops = StmtAddressList(),
			const RefAccessList& accesses = RefAccessList() ) : 
		core::NodeAnnotation(),
		iterVec(iv), 
		stmts(stmts),
		constraints( poly::cloneConstraint(iterVec, comb) ), // Switch the base to the this->iterVec 
		subScops(subScops), 
		accesses(accesses) { } 

	virtual std::ostream& printTo(std::ostream& out) const;

	inline const std::string& getAnnotationName() const { return NAME; }

	inline const utils::AnnotationKey* getKey() const { return &KEY; }

	inline bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, 
			const core::NodePtr& after) const 
	{ 
		return false; 
	}
	
	/**
	 * Return the iteration vector which is spawned by this region, and on which the associated
	 * constraints are based on.
	 */
	inline const poly::IterationVector& getIterationVector() const { return iterVec; }
	
	inline const ScopStmtList& getDirectRegionStmts() const { return stmts; }

	StmtScattering getStatementScattering() const;

	/** 
	 * Retrieves the constraint combiner associated to this ScopRegion.
	 */
	inline const poly::IterationDomain& getDomainConstraints() const { return constraints; }

	/** 
	 * Returns the set of ref accesses which are within this region (eventual access which are in
	 * sub regions are not returned by this function). Use listAccesses() to retrieve the complete
	 * list of accesses existing within this SCoP. 
	 */
	inline const RefAccessList& getDirectAccesses() const { return accesses; }

	
	const AccessInfoList listAccesses() const;

	/** 
	 *
	 * Returns the list of sub SCoPs which are inside this SCoP and introduce modification to the
	 * current iteration domain
	 */
	const StmtAddressList& getSubScops() const { return subScops; }

private:
	// Iteration Vector on which constraints are defined 
	poly::IterationVector iterVec;

	// List of statements direclty contained in this region (but not in nested sub-regions)
	ScopStmtList stmts;

	// List of constraints which this SCoP defines 
	poly::ConstraintCombinerPtr constraints;

	/**
	 * Ordered list of sub SCoPs accessible from this SCoP, the SCoPs are ordered in terms of their
	 * relative position inside the current SCoP
	 *  
	 * In the case there are no sub SCoPs for the current SCoP, the list of sub sub SCoPs is empty
	 */
	StmtAddressList subScops;

	// Access informations 
	RefAccessList accesses;

	boost::optional<AccessInfoList> cachedAccessInfo;
};

/**************************************************************************************************
 * AccessFunction : this annotation is used to annotate array subscript expressions with the
 * equality constraint resulting from the access function. 
 *
 * for example the subscript operation A[i+j-N] will generate an equality constraint of the type
 * i+j-N==0. Constraint which is used to annotate the expression.
 *************************************************************************************************/
class AccessFunction: public core::NodeAnnotation {
	poly::IterationVector 	iterVec;
	poly::Constraint  		eqCons;
public:
	static const string NAME;
	static const utils::StringKey<AccessFunction> KEY;

	AccessFunction(const poly::IterationVector& iv, const poly::Constraint& eqCons) : 
		core::NodeAnnotation(), 
		iterVec(iv), 
		eqCons( eqCons.toBase(iterVec) ) { assert(eqCons.getType() == poly::Constraint::EQ); }

	const std::string& getAnnotationName() const { return NAME; }

	const utils::AnnotationKey* getKey() const { return &KEY; }

	virtual std::ostream& printTo(std::ostream& out) const;

	const poly::Constraint& getAccessConstraint() const { return eqCons; }

	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, 
			const core::NodePtr& after) const { 
		return false; 
	}
};

typedef std::vector< std::pair<core::NodeAddress, poly::IterationVector> > ScopList;

/**************************************************************************************************
 * Finds and marks the SCoPs contained in the root subtree and returns a list of found SCoPs (an
 * empty list in the case no SCoP was found). 
 *************************************************************************************************/ 
ScopList mark(const core::NodePtr& root);

/**************************************************************************************************
 * printSCoP is a debug function which is used mainly as a proof of concept for the mechanism which
 * is used to support SCoPs. The function prints all the infromation related to a SCoP, i.e. for
 * each of the accesses existing in the SCoP the iteration domain associated to it and the access
 * function.
 *************************************************************************************************/
void printSCoP(std::ostream& out, const core::NodePtr& scop);

} // end namespace scop
} // end namespace analysis
} // end namespace insieme

