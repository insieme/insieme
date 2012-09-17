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
#include <functional>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint.h"

#include "insieme/analysis/cfg.h"
#include "insieme/analysis/tmp_var_map.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/utils/logging.h"

#include <boost/variant.hpp>

namespace insieme { 
namespace analysis { 

typedef utils::CombinerPtr<polyhedral::AffineFunction> ConstraintPtr;


struct NotAnAccessException : public std::logic_error {

	NotAnAccessException(const std::string& str) : 
		std::logic_error("Not an access: " + str) { }
};

// ================================================================================================
// ================================= UnifiedAddress ===============================================
// ================================================================================================
/**
 * An UnifiedAddress represents an abstraction which allows to address entities both in the IR and
 * in the CFG. 
 */
struct UnifiedAddress : public utils::Printable {
 
	typedef boost::variant<core::NodeAddress, cfg::Address> AddressImpl;
	

	UnifiedAddress(const core::NodeAddress& addr) : address(addr) { }

	UnifiedAddress(const cfg::Address& addr) : address(addr) { }


	bool isCFGAddress() const;

	core::NodeAddress getAbsoluteAddress(const TmpVarMap& varMap=TmpVarMap()) const;

	core::NodePtr getAddressedNode() const;

	UnifiedAddress getAddressOfChild(unsigned idx) const;

	template <class T>
	T as() const { 
		return boost::get<T>(address);
	}


	bool operator==(const UnifiedAddress& other) const {
		return getAbsoluteAddress(TmpVarMap()) == other.getAbsoluteAddress(TmpVarMap());
	}

	std::ostream& printTo(std::ostream& out) const {
		return out << address;
	}

	operator bool() const {
		if (isCFGAddress()) { return static_cast<bool>(boost::get<cfg::Address>(address)); }

		return boost::get<core::NodeAddress>(address);
	}

private:
	AddressImpl address;

};


//=== forward declarations ========================================================================
class Access;
typedef std::shared_ptr<const Access> AccessPtr;

class BaseAccess;
typedef std::shared_ptr<const BaseAccess> BaseAccessPtr;

class AccessDecorator;
typedef std::shared_ptr<const AccessDecorator> AccessDecoratorPtr;

class Deref;
typedef std::shared_ptr<const Deref> DerefPtr;

class Member;
typedef std::shared_ptr<const Member> MemberPtr;

class Subscript;
typedef std::shared_ptr<const Subscript> SubscriptPtr;


// ================================================================================================ 
// ========================================= Access ===============================================
// ================================================================================================ 

enum class AccessType { AT_BASE, AT_DEREF, AT_MEMBER, AT_SUBSCRIPT };

class Access { 

	UnifiedAddress addr;
	AccessType 	   type;

public:
	Access(const UnifiedAddress& addr, const AccessType& type) : 
		addr(addr), type(type)  { }

	const UnifiedAddress& getAddress() const { return addr; }

	const AccessType& getType() const { return type; }

	virtual bool isBaseAccess() const = 0;

	bool isReference() const {
		return addr.getAddressedNode().as<core::ExpressionPtr>()->getType()->getNodeType() == core::NT_RefType; 
	}

	virtual bool isContextDependent() const { return false; }

	bool operator==(const Access& other) const { 
		// Test the trivial case first 
		if (this == &other) { 
			return true;
		}
		return addr == other.addr;
	}

	virtual const BaseAccess& getRoot() const = 0;

	virtual ~Access() { }
};



class BaseAccess : public Access {

public:
	BaseAccess(const UnifiedAddress& addr) : Access(addr,AccessType::AT_BASE) { }

	core::VariablePtr getVariable() const { 
		return getAddress().getAddressedNode().as<core::VariablePtr>();
	}

	inline bool isBaseAccess() const { return true; }

	inline const BaseAccess& getRoot() const { return *this; }
};


// ================================================================================================ 
// ===================================== AccessDecorators =========================================
// ================================================================================================ 

struct AccessDecorator : public Access {

	AccessDecorator(const UnifiedAddress& 	addr, 
					const AccessPtr& 		subAccess, 
					const AccessType& 		type
	) :
		Access(addr, type), subAccess(subAccess) { }

	const AccessPtr& getSubAccess() const { return subAccess; }

	inline bool isBaseAccess() const { return false; }

	inline const BaseAccess& getRoot() const {
		return subAccess->getRoot();
	}

	virtual AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const = 0;

private:

	AccessPtr 	subAccess;
	AccessType 	type;
};


class Deref : public AccessDecorator {

public:
	Deref(const UnifiedAddress& 	addr, 
		  const AccessPtr& 			subAccess
	) :
		AccessDecorator(addr, subAccess, AccessType::AT_DEREF) { }

	inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
		return std::make_shared<Deref>(getAddress(), sub);
	}
};



class Member: public AccessDecorator {

	core::LiteralPtr	member;

public:
	Member(const UnifiedAddress& 	addr, 
		   const AccessPtr& 		subAccess, 
		   const core::LiteralPtr& 	member
	) :
		AccessDecorator(addr, subAccess, AccessType::AT_MEMBER), 
		member(member) { }

	inline const core::LiteralPtr& getMember() const { return member; }

	inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
		return std::make_shared<Member>(getAddress(), sub, member);
	}

};


class Subscript : public AccessDecorator {

	core::NodeAddress			ctx;
	polyhedral::IterationVector iv;
	ConstraintPtr 				range;

public:
	Subscript(const UnifiedAddress& 			addr, 
			const AccessPtr& 					subAccess, 
			const core::NodeAddress& 			ctx 	= core::NodeAddress(),
			const polyhedral::IterationVector& 	iterVec = polyhedral::IterationVector(), 
			const ConstraintPtr 				range 	= ConstraintPtr()
	) : 
		AccessDecorator(addr, subAccess, AccessType::AT_SUBSCRIPT), 
		ctx(ctx),
		iv(iterVec), 
		range( polyhedral::cloneConstraint(iv, range) ) { }

	
	const core::NodeAddress& getContext() const { return ctx; }

	const ConstraintPtr& getRange() const { return range; }

	bool isContextDependent() const {
		
		// we have an array access, now check whether we have a bound expression limiting the range of
		// accessed elements 
		if (range) {
			// if there are parameters in this access, then this access depends on the context 
			if (iv.getParameterNum() > 0) 
				return true; 
		}

		return false;
	}

	const polyhedral::IterationVector& getIterationVector() const {
		return iv;
	}
		
	inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
		return std::make_shared<Subscript>(getAddress(), sub, ctx, iv, range);
	}
};


AccessPtr switchRoot(const AccessPtr& access, const AccessPtr& newRoot);


template <class T>
std::shared_ptr<const T> cast(const AccessPtr& access) {
	return std::static_pointer_cast<const T>(access);
}

// ================================================================================================
// ===================================== AccessVisitors ===========================================
// ================================================================================================

template <class RetTy>
struct AccessVisitor {

	virtual RetTy visitBaseAccess(const BaseAccessPtr& access) = 0;

	virtual RetTy visitDecorator(const AccessDecoratorPtr& access) = 0;
	virtual RetTy visitDeref(const DerefPtr& access) = 0;
	virtual RetTy visitMember(const MemberPtr& access) = 0;
	virtual RetTy visitSubscript(const SubscriptPtr& access) = 0;

	virtual RetTy visit(const AccessPtr& access) = 0;

	virtual ~AccessVisitor() { }
};


template <class RetTy>
struct RecAccessVisitor : public AccessVisitor<RetTy> {

	RetTy visitBaseAccess(const BaseAccessPtr& access) = 0;

	RetTy visitDecorator(const AccessDecoratorPtr& access) {

		switch(access->getType()) {
		case AccessType::AT_DEREF: 		return visitDeref(cast<Deref>(access));
		case AccessType::AT_MEMBER:		return visitMember(cast<Member>(access));
		case AccessType::AT_SUBSCRIPT:	return visitSubscript(cast<Subscript>(access));

		default:
			assert(false && "Type of decorator not recognized");
		}
		return RetTy();
	}

	RetTy visitDeref(const DerefPtr& access) {
		return visitDecoratorImpl(access);
	}

	RetTy visitMember(const MemberPtr& access) {
		return visitDecoratorImpl(access);
	}

	RetTy visitSubscript(const SubscriptPtr& access) {
		return visitDecoratorImpl(access);
	}

	RetTy visit(const AccessPtr& access) {
	
		assert(access && "Visiting invalid access pointer");

		if (access->getType() == AccessType::AT_BASE) {
			// handles base-access nodes 
			return visitBaseAccess(cast<BaseAccess>(access));
		}

		// handles decorators, by redirecting the dispatch to the visitDecorator method 
		return visitDecorator(cast<AccessDecorator>(access));
	}

private:

	RetTy visitDecoratorImpl(const AccessDecoratorPtr& access) {
		visit(access->getSubAccess());
		return RetTy();
	}

};


bool equalPath(const AccessPtr& lhs, const AccessPtr& rhs);
	
class AccessManager;

class AccessClass;

typedef std::shared_ptr<AccessClass> AccessClassPtr;
typedef std::weak_ptr<AccessClass>   AccessClassWPtr;

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

	friend class insieme::analysis::AccessManager;

public:

	typedef std::pair<AccessClassWPtr, AccessDecoratorPtr> Dependence;

	typedef std::vector<Dependence> SubClasses;

private:
	/** 
	 * List of dependencies to sub-classes 
	 */
	SubClasses subClasses;

	/**
	 * Reference to the parent class 
	 */
	AccessClassWPtr parentClass;


	/** 
	 * AccessClass instances can only be created by the AccessMaanger class
	 */
	AccessClass(
			const std::reference_wrapper<const AccessManager>& mgr, 
			size_t uid, 
			const AccessClassWPtr parent = AccessClassWPtr()
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

	AccessClass& storeAccess(const AccessPtr& access) {
		/** 
		 * Makes sure the access is not already in this class
		 */
		assert(!contains(access) && "Access is already present in this class");
		accesses.push_back(access); 
		return *this;
	}

	/** 
	 * Look for an access inside this class, the lookup is implemented by comparing for equivalence
	 * between accesses 
	 */
	bool contains(const AccessPtr& access) {
		auto fit = std::find_if( accesses.begin(), accesses.end(), 
						std::bind(equal_target<AccessPtr>(), std::placeholders::_1, access) );
		return fit != accesses.end();
	}

	/** 
	 * Return the unique identifier used to identify this access class.
	 *
	 * Comparison based on identifier is valid only within the same access manager.
	 */
	inline size_t getUID() const { return uid; }

	inline void setParentClass(const AccessClassPtr& parent) { 
		this->parentClass = parent; 
	}

	const AccessClassPtr getParentClass() const {
		return parentClass.lock();
	}

	inline void addSubClass(const Dependence& dep) {
		subClasses.push_back(dep);
	}

	const SubClasses& getSubClasses() const { return subClasses; }
	SubClasses& getSubClasses() { return subClasses; }

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
std::set<core::ExpressionAddress> 
extractRealAddresses(const AccessClass& cl, const TmpVarMap& tmpVarMap = TmpVarMap());


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

	AccessClassPtr getClassFor(const AccessPtr& access);

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

	std::ostream& printTo(std::ostream& out) const;

};

AccessPtr 
getImmediateAccess(core::NodeManager& mgr, const UnifiedAddress& expr, const TmpVarMap& tmpVarMap=TmpVarMap());


inline AccessPtr 
getImmediateAccess(core::NodeManager& mgr, const core::NodeAddress& expr, const TmpVarMap& tmpVarMap=TmpVarMap()) {
	return getImmediateAccess(mgr, UnifiedAddress(expr), tmpVarMap);
}

inline AccessPtr 
getImmediateAccess(core::NodeManager& mgr, const cfg::Address& expr, const TmpVarMap& tmpVarMap=TmpVarMap()) {
	return getImmediateAccess(mgr, UnifiedAddress(expr), tmpVarMap);
}

} } // end insieme::analysis namespace 

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::analysis::AccessPtr& access);

}// end std namespace 
