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

#include <iterator>
#include <stdexcept>
#include <memory>
#include <set>
#include <list>

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/constraint.h"
#include "insieme/analysis/defuse_collect.h"

#include "insieme/core/ir_node.h"

#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"
#include "boost/mpl/or.hpp"

namespace insieme {
namespace analysis {
namespace poly {

typedef ConstraintCombinerPtr<AffineFunction> AffineConstraintPtr;

/**************************************************************************************************
 * IterationDomain: the iteration domain represent the domain on which a statement is valid.
 * Therefore it is a represented by a set of constraints (ConstraintCombiner). However, the
 * iteration domain also allows the creation of empty and universe sets which are used to represent
 * statement which are not bound by any constraint
 **************************************************************************************************/
class IterationDomain : public utils::Printable {

	const IterationVector& iterVec;
	AffineConstraintPtr  constraint;
	bool empty;

public:
	IterationDomain( const IterationVector& iterVec, bool empty=false) : 
		iterVec(iterVec), empty(empty) { }

	explicit IterationDomain( const AffineConstraintPtr& constraint ) : 
		iterVec( extractIterationVector(constraint) ), 
		constraint(constraint), 
		empty(false) { }

	IterationDomain( const IterationVector& iv, const IterationDomain& otherDom) : 
		iterVec(iv), 
		// update the iteration vector of the important constraint to iv
		constraint( poly::cloneConstraint(iv, otherDom.constraint ) ), 
		empty(false) { }
	
	inline const IterationVector& getIterationVector() const { 
		return iterVec; 
	}

	inline const AffineConstraintPtr& getConstraint() const { 
		return constraint;
	}

	inline bool isUniverse() const { return !empty && !static_cast<bool>(constraint); }

	inline bool isEmpty() const { return empty; }

	// Intersect two iteration domains and return assign the result to this iteration domain
	IterationDomain& operator&=(const IterationDomain& other) {
		assert(iterVec == other.iterVec);
		constraint = constraint and other.constraint;
		return *this;
	}

	std::ostream& printTo(std::ostream& out) const;
};

IterationDomain operator&&(const IterationDomain& lhs, const IterationDomain& rhs);
IterationDomain operator||(const IterationDomain& lhs, const IterationDomain& rhs);
IterationDomain operator!(const IterationDomain& other);

/**************************************************************************************************
 * AffineSystem: represents a set of affine functions. The invariant is that every affine function
 * composing an affine system refers to the same iteration vector. Therefore changes to the
 * iteration vector owned by this affine system results in changes to all the affine functions. 
 *************************************************************************************************/
class AffineSystem : public utils::Printable, boost::noncopyable {
	
	typedef std::unique_ptr<AffineFunction> AffineFunctionPtr;
	typedef std::vector<AffineFunctionPtr> 	AffineList;

	const IterationVector& iterVec; 
	AffineList funcs;

public:

	template <class T, class IterT>
	class Iterator : public boost::random_access_iterator_helper<Iterator<T, IterT>, T> {
		IterT it, end;

	public:
		Iterator(const IterT& begin, const IterT& end): it(begin), end(end) { }

        T& operator*() const { return **it; }

        Iterator<T, IterT>& operator++() { ++it; return *this; }
		Iterator<T, IterT>& operator+=(size_t val) { it+=val; return *this; }

        bool operator==(const Iterator<T, IterT>& rhs) const { 
			return it == rhs.it;
		}

		const IterT& get() const { return it; }
	};

	typedef Iterator<AffineFunction, AffineList::iterator> iterator;
	typedef Iterator<const AffineFunction, AffineList::const_iterator> const_iterator;

	AffineSystem(const IterationVector& iterVec) : 	
		iterVec(iterVec) { }	

	AffineSystem(const AffineSystem& other) : iterVec(other.iterVec) { 
		for_each(other.funcs, [&] (const AffineFunctionPtr& cur) { 
				this->append(*cur); 
			} );
		assert( other.funcs.size() == funcs.size() );
	}

	inline const IterationVector& getIterationVector() const { 
		return iterVec;
	}

	inline void append(const AffineFunction& af) { 
		assert( &iterVec == &af.getIterationVector() && 
			"AffineFunction has to have the same iteration vector" );

		return insert(end(), af);
	}

	void insert(const iterator& pos, const AffineFunction& af);

	inline size_t size() const { return funcs.size(); }

	inline iterator begin() { return iterator(funcs.begin(), funcs.end()); }
	inline iterator end() { return iterator(funcs.end(), funcs.end()); }

	inline const_iterator begin() const { return const_iterator(funcs.cbegin(), funcs.cend()); }
	inline const_iterator end() const { return const_iterator(funcs.cend(), funcs.cend()); }

	// Return the Affine function at position N of this Affine system 
	AffineFunction& operator[]( size_t n ) { 
		assert( n < funcs.size() && "Index out of array bounds" );
		return *funcs[n]; 
	}
	const AffineFunction& operator[]( size_t n ) const { 
		assert( n < funcs.size() && "Index out of array bounds" );
		return *funcs[n];
	}

	std::ostream& printTo(std::ostream& out) const;

};

typedef std::shared_ptr<AffineSystem> AffineSystemPtr;

/********************************************************************************************** 
 * AccessInfo is a tuple which gives the list of information associated to a ref access: i.e.
 * the pointer to a RefPtr instance (containing the ref to the variable being accessed and the
 * type of access (DEF or USE). The iteration domain which defines the domain on which this
 * access is defined and the access functions for each dimensions.
 *********************************************************************************************/
class AccessInfo : public utils::Printable {

	core::ExpressionAddress	expr;
	Ref::RefType			type;
	Ref::UseType			usage; 
	poly::AffineSystemPtr	access;

public:
	AccessInfo(
		const core::ExpressionAddress&	expr, 
		const Ref::RefType& 			type, 
		const Ref::UseType& 			usage, 
		const poly::AffineSystemPtr&	access 
	) 
	: expr(expr), type(type), usage(usage), access(access) { }
	
	inline const core::ExpressionAddress& getExpr() const { return expr; }
	inline const Ref::RefType& getRefType() const { return type; }
	inline const Ref::UseType& getUsage() const { return usage; }

	inline AffineSystemPtr& getAccess() { return access; }
	inline const AffineSystemPtr& getAccess() const { return access; }

	std::ostream& printTo(std::ostream& out) const;
};

typedef std::vector<AccessInfo> AccessList;

/*********************************************************************************************
 * This class contains the scattering information of a statement contained in this SCoP from a
 * point of view of iteration vector of this entry point. For each statement we keep:
 * @addr: address (relative to this root node), 
 *
 * @iterDom:    iteration domain which contains the domain information for which the statment is 
 * 			    defined
 * @scattering: Which is the scattering infromation with the relative ordering of the statement 
 * 				within this region
 * @accessList: The list of ref accesses within the statement rewritten to the iteration vector
 * 				of this entry point (iterVec).
 *********************************************************************************************/

/**************************************************************************************************
 * PolyStmt: this class assembles together the informations which are utilized to represent a
 * statement in the polyhedral mdoel. 
 *
 * A statement is represented by the following infomrations:
 * 	- An *iteration domain* (with associated an iteration vector)
 * 	- A *scheduling* (or scattering) matrix (which defines the logical dates when the statment is
 * 	  executed)
 *  - a set of *access functions* which identifies the read/writes happening inside the statement
 *************************************************************************************************/
class Stmt : public utils::Printable {

	size_t	 				id;
	core::StatementAddress 	addr;
	IterationDomain 		dom;
	AffineSystem 			schedule;
	AccessList				access;

	typedef AccessList::iterator AccessIterator;
	typedef AccessList::const_iterator ConstAccessIterator;
public:

	Stmt(
		  size_t 						id, 
		  const core::StatementAddress& addr,
		  const IterationDomain& 		dom, 
		  const AffineSystem& 			schedule, 
		  const AccessList& 			access 
		) 
	: id(id), addr(addr), dom(dom), schedule(schedule), access(access) { }

	// Getter for the ID
	inline size_t getId() const { return id; }

	// Getter for StmtAddress
	inline const core::StatementAddress& getAddr() const { return addr; }

	// Getters/Setters for the iteration domain
	inline const IterationDomain& getDomain() const { return dom; }
	inline IterationDomain& getDomain() { return dom; }

	// Getters/Setters for scheduling / scattering 
	inline AffineSystem& getSchedule() { return schedule; }
	inline const AffineSystem& getSchedule() const { return schedule; }

	// Accessories for iterating through accesses of this statement (read/write)
	inline AccessIterator access_begin() { return access.begin(); }
	inline AccessIterator access_end() { return access.end(); }

	// Accessories for iterating through accesses of this statement (write only)
	inline ConstAccessIterator access_begin() const { return access.cbegin(); }
	inline ConstAccessIterator access_end() const { return access.cend(); }

	std::ostream& printTo(std::ostream& out) const;
};

struct Scop : public std::list<Stmt> {

	Scop(const IterationVector& iterVec) : iterVec(iterVec) { }

private:
	IterationVector 	iterVec;
};

} // end poly namespace
} // end analysis namespace
} // end insieme namespace 




