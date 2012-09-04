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

#include <set>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint.h"

#include "insieme/analysis/cfg.h"
#include "insieme/analysis/tmp_var_map.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/utils/logging.h"

namespace insieme { 
namespace analysis { 

struct NotAnAccessException : public std::logic_error {

	bool isLit;

	NotAnAccessException(bool isLit) :
		std::logic_error("not an access"), isLit(isLit) { }
};

typedef utils::CombinerPtr<polyhedral::AffineFunction> ConstraintPtr;

enum class VarType { SCALAR, MEMBER, TUPLE, ARRAY };

/** 
 * The Access class represent an access to an IR variable. 
 *
 * This could be a scalar, array, tuple or member variable access. 
 *
 * Because we want to represent the concept of V L-Values, this class can hold both RefTypes and not
 * ref types, therefore anything which can appear of the left hand side of an assignment operation
 * in C or a declaration statement. 
 */
class Access : public utils::Printable {

	// Address of the access represented by this object 
	//
	// This address is an absolute address if no CFG block is provided, otherwise it will be an
	// address relative to the cfgBlock. This is required in order to be able to address temporary
	// variable being created during the construction of the CFG which cannot be addressed based on
	// the root of the CFG. For this reason, in order to address a specific location of the CFG we
	// base it on the actual block.
	core::ExpressionAddress 	base_expr;

	// Actuall variable being accessed (note that this variable might be an alias)
	core::VariablePtr			variable;

	/**
	 * Path to the accessed member/element/component 
	 *  => For scalar, the path is empty 
	 */ 
	core::datapath::DataPathPtr	path;

	// The type of this access
	VarType 					type;

	// The cfg-block containing this access 
	cfg::BlockPtr 				cfgBlock;
	// Index relative to the cfg block 
	size_t						stmtIdx;

	polyhedral::IterationVector iterVec;

    /**
     * Represents the domain/range on which this access is defined
     *
     * For scalars or members the domain the domain is not defined
	 *
     * For arrays the domain depends on the range of values being accessed. It could be either a
     * single element or strided domain in the circumstances the array is accessed inside a loop
     */
	 ConstraintPtr 				array_access;

	/** 
	 * A constraint has sense only if within a SCoP. Indeed, two equal constraints extracted from
	 * two different SCoPs based on the same variables may not be referring to the same memory
	 * location as the indexes may be reassigned between the two SCoPs
	 */
	core::NodeAddress ctx;

	Access(const core::ExpressionAddress& 		expr, 
		   const core::VariablePtr& 			var,
		   const core::datapath::DataPathPtr& 	path, 
		   const VarType& 						type,
		   const cfg::BlockPtr& 				cfg_block = cfg::BlockPtr(),
		   size_t								stmtIdx = 0,
		   const polyhedral::IterationVector&	iv = polyhedral::IterationVector(), 
		   const ConstraintPtr& 				dom = ConstraintPtr(),
		   const core::NodeAddress& 			ctx = core::NodeAddress() 
	) :
		base_expr(expr), 
		variable(var),
		path(path), 
		type(type),
		cfgBlock(cfg_block),
		stmtIdx(stmtIdx),
		iterVec(iv),
		array_access( cloneConstraint(iterVec, dom) ),
		ctx(ctx) {  }

	friend Access getImmediateAccess(
			const core::ExpressionAddress& expr, 
			const std::pair<cfg::BlockPtr, size_t>& cfgAddr, 
			const TmpVarMap& tmpVarMap
		);

public:
	
	inline const VarType& getType() const { return type; }

	/**
	 * Tells whtether this usage of the variable is an rvalue.
	 * This is happens for example when a ref<'a> is dereferenced, or the type 
	 * of the variable is a non ref. 
	 */
	bool isRef() const;

	inline core::VariablePtr getAccessedVariable() const { return variable; }

	inline core::ExpressionAddress getAccessExpression() const { return base_expr; }

	inline cfg::BlockPtr getCFGBlock() const { return cfgBlock; }

	inline size_t getStmtIdx() const { return stmtIdx; }

	inline const core::datapath::DataPathPtr& getPath() const {	return path; }

	/** 
	 * If this is an array access, it may have associated a constraint which states the range of
	 * elements being accessed
	 */
	inline const ConstraintPtr& getAccessedRange() const { return array_access; }

	/** 
	 * Return the context on which the constraint has validity
	 */
	inline const core::NodeAddress& getContext() const { return ctx; }

	/** 
	 * Accesses to arrays have range informations which states the subset of elements of the array
	 * being accessed by the particular expression (e.g. an array access inside a loop). It could
	 * happen that these accesses are symbolically bound to variables. 
	 *
	 * When comparing two accesses we must be sure that if the bounds are symbolic, they belong to
	 * the same SCoP which makes sure that the bound is not being assigned. In the case of
	 * comparison of two symbolically bound accesses belonging to different context, then we have to
	 * assume that there is a potential collision between elements. 
	 *
	 * However, if the bounds of the range are not symbolical, then we can compare accesses
	 * belonging to different contexts as long as the accessed array is the same
	 */
	bool isContextDependent() const;

	std::ostream& printTo(std::ostream& out) const;
	
	// TO BE REMOVED 
	bool operator<(const Access& other) const ;

	// Determine whether two accesses are the same access, this can be determined by comparing the
	// address of the accessed variable  
	inline bool operator==(const Access& other) const {
		// handles the trivial case 
		if (this == &other) { return true; }

		return ((cfgBlock && other.cfgBlock && *cfgBlock == *other.cfgBlock) || 
				(!cfgBlock && !other.cfgBlock)
			   ) && base_expr.getRootNode() == other.base_expr.getRootNode() 
				 && base_expr == other.base_expr;
	}

	bool operator!=(const Access& other) const { return !(*this == other); }
};

typedef std::shared_ptr<Access> AccessPtr;


/** 
 * Given two accesses, this function returns true if the ranges on which the accesses are defined
 * are overlapping or not.
 */
// bool isOverlapping(const Access& a1, const Access& a2);

/** 
 * Given an expression, it returns the immediate memory access represented by this expression. 
 *
 * The method always returns the imediate access and in the case of expression accessing multiple
 * variables, only the immediate access will be returned. 
 */
Access getImmediateAccess(const core::ExpressionAddress& expr, 
						  const std::pair<cfg::BlockPtr, size_t>& cfgAddr = std::make_pair(cfg::BlockPtr(),0), 
						  const TmpVarMap& tmpVarMap=TmpVarMap()
);


/** 
 * Given a statement, this function takes care of extracting all memory accesses within that
 * statement. 
 */
std::set<Access> extractFromStmt(const core::StatementAddress& stmt, const TmpVarMap& tmpVarMap=TmpVarMap());

/**
 * Similar to the previous function, this function collects all memory accesses within a statement,
 * accesses will be append to the provided set 
 */
void extractFromStmt(const core::StatementAddress& stmt, 
					 std::set<Access>& accesses, 
					 const TmpVarMap& tmpVarMap=TmpVarMap()
					);

/**
 * States whether two accesses are conflicting, it returns true if the 2 accesses referes to the
 * same variable, and the datapaths of the two variables are overlapping. The predicate also
 * receives the alias map as argument so that it includes aliasing when checking for conflicts
 */
bool isConflicting(const Access& acc1, const Access& acc2, const TmpVarMap& tmpVarMap = TmpVarMap());

class AccessManager;

class AccessClass;

typedef std::shared_ptr<AccessClass> AccessClassPtr;

/** 
 * An access class is a set of accesses which refer to the same memory location. In case of R-Values
 * an access refers to the actual value. Important to notice that access classes are specific to a
 * program point (represented by a CFG blok)
 *
 * An access can refer to larger section of memory (in case of array accesses inside loopbounds), in
 * that case a class contains all the accesses which may have a conflict.
 *
 * Accesses classes are meant to be used in DF analysis and be stored into sets, which means that
 * they provide a partial order. 
 */
class AccessClass : public utils::Printable {

	std::reference_wrapper<const AccessManager> mgr;

	size_t uid;

public:
	typedef std::vector<AccessPtr> AccessVector;

private:
	/**
	 * Stores the accesses which refer to a memory area
	 */
	AccessVector accesses;

public:

	typedef std::pair<AccessClassPtr, core::datapath::DataPathPtr> Dependence;

	typedef std::vector<Dependence> SubClasses;

private:
	/** 
	 * List of dependencies to sub-classes 
	 */
	SubClasses subClasses;

	/**
	 * Reference to the parent class 
	 */
	const AccessClassPtr parentClass;

	friend class AccessManager;

	/** 
	 * AccessClass instances can only be created by the AccessMaanger class
	 */
	AccessClass(
			const std::reference_wrapper<const AccessManager>& mgr, 
			size_t uid, 
			const AccessClassPtr parent = AccessClassPtr()
	) : mgr(mgr), 
		uid(uid), 
		parentClass(parent) {  }

public:

	AccessClass(const AccessClass& other): 
		mgr(other.mgr), 
		uid(other.uid), 
		accesses(other.accesses), 
		parentClass(other.parentClass) { }

	AccessClass(AccessClass&& other) : 
		mgr(other.mgr), 
		uid(other.uid), 
		accesses(std::move(other.accesses)),
		parentClass(std::move(other.parentClass)) { }

	AccessClass& storeAccess(const Access& access) {
		/** 
		 * Makes sure the access is not already in this class
		 */
		assert(std::find_if(accesses.begin(), accesses.end(), 
					[&](const AccessPtr& cur){ return *cur == access; }) == accesses.end() 
				&& "Access is already present in this class");

		accesses.push_back(std::make_shared<Access>(access)); 

		return *this;
	}

	/** 
	 * Return the unique identifier used to identify this access class.
	 *
	 * Comparison based on identifier is valid only within the same access manager.
	 */
	inline size_t getUID() const { return uid; }

	const AccessClassPtr getParentClass() const {
		return parentClass;
	}

	inline void addSubClass(const Dependence& dep) {
		subClasses.push_back(dep);
	}

	const SubClasses& getSubClasses() const { return subClasses; }

	inline bool operator<(const AccessClass& other) const { 
		return uid < other.uid;
	}

	inline bool operator==(const AccessClass& other) const { 
		if (this == &other) { return true; }

		// check if the ID is the same and was generated by the same manager 
		return &mgr.get() == &other.mgr.get() && uid == other.getUID();
	}

	inline bool operator!=(const AccessClass& other) const {
		return !(*this == other);
	}

	std::ostream& printTo(std::ostream& out) const;

	inline AccessVector::const_iterator begin() const { return accesses.begin(); }
	inline AccessVector::const_iterator end() const { return accesses.end(); }

	inline size_t size() const { return accesses.size(); }
};


/** 
 * Return the vector of addresses which are not temporary variable and therefore it returns
 * addresses which exists only outside the CFG
 */
std::set<core::ExpressionAddress> extractRealAddresses(const AccessClass& cl, const TmpVarMap& tmpVarMap = TmpVarMap());


/**
 * The AccessManager takes care of managing and creating access classes 
 */
class AccessManager: public utils::Printable {

public:
	typedef std::vector<AccessClassPtr> ClassVector;

private:
	ClassVector classes;
	
	const CFG* 			cfg;
	const TmpVarMap& 	tmpVarMap;

public:

	typedef ClassVector::iterator 		iterator;
	typedef ClassVector::const_iterator const_iterator;

	AccessManager(const CFG* cfg = nullptr, const TmpVarMap& tmpVarMap = TmpVarMap()) : 
		cfg(cfg), 
		tmpVarMap(tmpVarMap) { }

	AccessClassPtr getClassFor(const Access& access);

	inline iterator begin() { return classes.begin(); }
	inline iterator end() { return classes.end(); }

	inline const_iterator begin() const { return classes.begin(); }
	inline const_iterator end() const { return classes.end(); }

	inline AccessClass& operator[](const size_t uid) { 
		assert(uid < classes.size() && "OutOfBounds array access");
		return *classes[uid]; 
	}

	inline const AccessClass& operator[](const size_t uid) const {
		assert(uid < classes.size() && "OutOfBounds array access");
		return *classes[uid]; 
	}

	inline size_t size() const { return classes.size(); }

	inline std::ostream& printTo(std::ostream& out) const { 
		return out << "AccessManager [" << size() << "]\n\t" << 
			join("\n\t", classes, [&](std::ostream& jout, const AccessClassPtr& cur) { jout << *cur; }) << "]";
	}

};

} } // end insieme::analysis::dfa namespace 
