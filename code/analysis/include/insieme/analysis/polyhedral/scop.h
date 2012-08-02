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

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_address.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/defuse_collect.h"

#include "boost/optional/optional.hpp"

#include "insieme/utils/exception_utils.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {

namespace core { namespace arithmetic {
class Formula;
} } // end core::arithmetic namespace

namespace analysis { namespace polyhedral { namespace scop {

/**************************************************************************************************
 * Expection which is thrown when a particular tree is defined to be not a static control part. This
 * exception has to be forwarded until the root containing this node which has to be defined as a
 * non ScopRegion
 * 
 * Because this exception is only used within the implementation of the ScopRegion visitor, it is
 * defined in the anonymous namespace and therefore not visible outside this translation unit.
 *************************************************************************************************/
class NotASCoP : public insieme::utils::TraceableException {
	core::NodePtr root;
	std::string msg;

public:
	template <class SubExTy>
	NotASCoP(const std::string& 	     msg, 
			 const char*		 	 ex_type,
			 const char*	 	   file_name, 
			 int 					 line_no, 
			 const SubExTy*			  sub_ex,
			 const core::NodePtr& 	    root) 
	: TraceableException(msg, ex_type, file_name, line_no, sub_ex), 
	  root(root) 
	{ 
		if (!sub_ex) {
			std::ostringstream ss;
			ss << "Node: \n" << insieme::core::printer::PrettyPrinter(root) 
			   << "\nNot a Static Control Part:\n" << msg;
			setMessage(ss.str());
		}
	}
	
	const core::NodePtr& node() const { return root; }

	virtual ~NotASCoP() throw() { }
};

typedef std::vector<core::NodeAddress> 					AddressList;
typedef std::pair<core::NodeAddress, IterationDomain> 	SubScop;
typedef std::list<SubScop> 								SubScopList;

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
struct ScopRegion: public core::NodeAnnotation {

	static const string NAME;
	static const utils::StringKey<ScopRegion> 	KEY;

	// This class keeps the information on how a particular reference is accessed.  This is slightly
	// different from the DefUse::Ref class as this one also include information related to the
	// conversion of the reference into the polyhedral model 
	struct Reference : public boost::noncopyable {
		core::ExpressionAddress 	 		refExpr;
		Ref::UseType						usage;
		Ref::RefType						type;
		std::vector<core::ExpressionPtr> 	indecesExpr;
		IterationVector						iterVec;
		AffineConstraintPtr			 		range;

		Reference(const core::ExpressionAddress& 		refExpr, 
				const Ref::UseType& 					usage, 
				const Ref::RefType& 					type,
				const std::vector<core::ExpressionPtr>& indecesExpr,
				const IterationVector&					iv = IterationVector(),
				const AffineConstraintPtr& 				range = AffineConstraintPtr())
		: refExpr(refExpr), 
		  usage(usage), 
		  type(type), 
		  indecesExpr(indecesExpr), 
		  iterVec(iv), 
		  range( cloneConstraint(iterVec,range) ) 
		{  }
	};

	typedef std::shared_ptr<Reference> ReferencePtr;

	/**************************************************************************************************
	 * ScopRegion::Stmt: Utility class which contains all the information of statements inside a SCoP (both
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
	struct Stmt { 

		// Set of array accesses which appears strictly within this SCoP, array access in sub SCoPs will
		// be directly referred from sub SCoPs. The accesses are ordered by the appearance in the SCoP
		typedef std::vector<ReferencePtr> RefAccessList;

		Stmt(const core::StatementAddress& addr, const RefAccessList& accesses) : 
			address(addr), accesses(accesses) { }

		inline const core::StatementAddress& getAddr() const { return address; }
		
		inline const core::StatementAddress& operator->() const { return address; }

		inline bool operator<(const Stmt& other) const { return address < other.address; }

		virtual RefAccessList getRefAccesses() const { return accesses; } 

	private:
		core::StatementAddress 		address;
		RefAccessList 				accesses;
	};
	
	typedef std::vector<Stmt> 			StmtVect;

	typedef std::vector<Iterator> 		IteratorOrder;
	
	ScopRegion( const core::NodePtr& 	annNode,
				const IterationVector& 	iv, 
				const IterationDomain& 	comb,
				const StmtVect&		 	stmts = StmtVect(),
				const SubScopList& 		subScops_ = SubScopList() 
			  ) :
		annNode(annNode),
		iterVec(iv), 
		stmts(stmts),
		domain( iterVec, comb ), // Switch the base to the this->iterVec 
		valid(true)
	{ 
		
		for_each(subScops_.begin(), subScops_.end(), 
			[&] (const SubScop& cur) { 
				subScops.push_back( SubScop(cur.first, IterationDomain(iterVec, cur.second)) ); 
			});	
	} 

	std::ostream& printTo(std::ostream& out) const;

	inline const std::string& getAnnotationName() const { return NAME; }

	inline const utils::AnnotationKey* getKey() const { return &KEY; }

	inline bool migrate(const core::NodeAnnotationPtr& ptr, 
						const core::NodePtr& before, 
						const core::NodePtr& after) const { return false; }
	
	
	inline bool isResolved() const { return static_cast<bool>(scopInfo); }

	/**
	 * Return the iteration vector which is spawned by this region, and on which the associated
	 * constraints are based on.
	 */
	inline const IterationVector& getIterationVector() const {  return iterVec; }
	
	/** 
	 * Retrieves the constraint combiner associated to this ScopRegion.
	 */
	inline const IterationDomain& getDomainConstraints() const { return domain; }

	inline const StmtVect& getDirectRegionStmts() const { return stmts; }

	/**
	 * Returns the iterator through the statements and sub statements on this SCoP. 
	 * For each statements the infromation of its iteration domain, scattering matrix 
	 * and access functions are listed. 
	 */
	inline const Scop& getScop() const {
		assert(isValid() && "SCoP is not valid");
		if (!isResolved()) { resolve(); }
		return *scopInfo;
	}

	inline Scop& getScop() {
		assert(isValid() && "SCoP is not valid");
		if (!isResolved()) { resolve(); }
		return *scopInfo;		
	}

	/** 
	 * Returns the list of sub SCoPs which are inside this SCoP and introduce modification to the
	 * current iteration domain
	 */
	const SubScopList& getSubScops() const { return subScops; }

	bool containsLoopNest() const;

	inline bool isValid() const { return valid; }
	inline void setValid(bool value) { valid = value; }

	static boost::optional<Scop> toScop(const core::NodePtr& root);

private:

	/**
	 * Resolve the SCoP, this means adapt all the access expressions on nested SCoPs to this level
	 * and cache all the scattering info at this level
	 */
	void resolve() const;


	const core::NodePtr annNode;

	// Iteration Vector on which constraints of this region are defined 
	IterationVector iterVec;

	// List of statements direclty contained in this region (but not in nested sub-regions)
	StmtVect stmts;

	// List of constraints which this SCoP defines 
	IterationDomain domain;

	/**
	 * Ordered list of sub SCoPs accessible from this SCoP, the SCoPs are ordered in terms of their
	 * relative position inside the current SCoP
	 *  
	 * In the case there are no sub SCoPs for the current SCoP, the list of sub sub SCoPs is empty
	 */
	SubScopList subScops;

	mutable std::shared_ptr<Scop> scopInfo;

	bool valid;
};

/**************************************************************************************************
 * AccessFunction : this annotation is used to annotate array subscript expressions with the
 * equality constraint resulting from the access function. 
 *
 * for example the subscript operation A[i+j-N] will generate an equality constraint of the type
 * i+j-N==0. Constraint which is used to annotate the expression.
 *************************************************************************************************/
class AccessFunction: public core::NodeAnnotation {
	IterationVector 	iterVec;
	AffineFunction 	access;
public:
	static const string NAME;
	static const utils::StringKey<AccessFunction> KEY;

	AccessFunction(const IterationVector& iv, const AffineFunction& access) : 
		core::NodeAnnotation(), 
		iterVec(iv), 
		access( access.toBase(iterVec) ) { }

	inline const std::string& getAnnotationName() const { return NAME; }

	inline const utils::AnnotationKey* getKey() const { return &KEY; }

	std::ostream& printTo(std::ostream& out) const;

	inline const AffineFunction& getAccessFunction() const { return access; }
	
	inline const IterationVector& getIterationVector() const { return iterVec; }

	inline bool migrate(const core::NodeAnnotationPtr& ptr, 
						const core::NodePtr& before, 
						const core::NodePtr& after) const 
	{ 
		return false; 
	}
};

/**************************************************************************************************
 * Finds and marks the SCoPs contained in the root subtree and returns a list of found SCoPs (an
 * empty list in the case no SCoP was found). 
 *************************************************************************************************/ 
AddressList mark(const core::NodePtr& root);

inline boost::optional<Scop> ScopRegion::toScop(const core::NodePtr& root) {
	AddressList&& al = insieme::analysis::polyhedral::scop::mark(root);
	if(al.empty() || al.size() > 1 || al.front().getDepth() > 1) { 
		// If there are not scops or the number of scops is greater than 2 
		// or the the extracted scop is not the top level node 
		return boost::optional<Scop>();
	}
	assert(root->hasAnnotation(ScopRegion::KEY));
	ScopRegion& ann = *root->getAnnotation(ScopRegion::KEY);

	ann.resolve();

	if (!ann.isValid()) { 
		return boost::optional<Scop>(); 
	}
	return boost::optional<Scop>( ann.getScop() );
}


} } } } // end insieme::analysis::polyhedral::scop namespace

