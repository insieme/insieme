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
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/analysis/defuse_collect.h"
#include "insieme/analysis/dep_graph.h"

#include "insieme/core/ir_node.h"

#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"
#include "boost/mpl/or.hpp"

namespace insieme {
namespace analysis {
namespace poly {

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

	/**
	 * Conctructs an iteration domain from a combined constraint
	 */
	explicit IterationDomain( const AffineConstraintPtr& constraint ) : 
		iterVec( extractIterationVector(constraint) ), 
		constraint(constraint), 
		empty(false) { }

	/**
	 * Constructs an iteration domain from a simple constraint
	 */
	explicit IterationDomain( const AffineConstraint& constraint ) : 
		iterVec( constraint.getFunction().getIterationVector() ), 
		constraint( makeCombiner(constraint) ), 
		empty(false) { }

	/**
	 * Constructs an IterationDomain by copy updating to iterVec iteration vector 
	 */
	IterationDomain( const IterationVector& iv, const IterationDomain& otherDom ) : 
		iterVec(iv), 
		// update the iteration vector of the important constraint to iv
		constraint( poly::cloneConstraint(iv, otherDom.constraint ) ), 
		empty(false) { }
	
	/**
	 * Builds an iteration domain starting from an iteration vector and a coefficient matrix
	 */
	template <class MatTy>
	IterationDomain( const IterationVector& iv, const MatTy& coeffs ) : 
		iterVec(iv), empty(coeffs.empty()) 
	{
		if ( coeffs.empty() ) { return;	}

		constraint = makeCombiner(AffineConstraint(AffineFunction(iterVec, coeffs.front())));
		for_each( coeffs.begin()+1, coeffs.end(), [&] (const typename MatTy::value_type& cur) { 
			constraint = constraint and AffineConstraint( AffineFunction(iterVec, cur) );
		} );

		assert(constraint);
	}

	inline const IterationVector& getIterationVector() const { return iterVec; }
	inline const AffineConstraintPtr& getConstraint() const { return constraint;}

	inline bool isUniverse() const { return !empty && !static_cast<bool>(constraint); }

	inline bool isEmpty() const { return empty; }

	// Intersect two iteration domains and return assign the result to this iteration domain
	inline IterationDomain& operator&=(const IterationDomain& other) {
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

	// Defines an iterator used to visit the Affine functions contained in this system
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

	// Creates an empty affine system based on the iteration vector itervec
	AffineSystem(const IterationVector& iterVec) : 	iterVec(iterVec) { }	

	AffineSystem(const AffineSystem& other) : iterVec(other.iterVec) { 
		for_each(other.funcs, [&] (const AffineFunctionPtr& cur) { this->append(*cur); } );
		assert( other.funcs.size() == funcs.size() );
	}

	AffineSystem(const IterationVector& iterVec, const AffineSystem& other) : 
		iterVec(iterVec) 
	{
		for_each(other.funcs, [&] (const AffineFunctionPtr& cur) { this->append( *cur ); } );
		assert( other.funcs.size() == funcs.size() );
	}

	template <class MatTy>
	AffineSystem(const IterationVector& iterVec, const MatTy& coeffs) : 
		iterVec(iterVec) 
	{
		if ( coeffs.empty() ) { return; }
		for_each(coeffs, [&](const typename MatTy::value_type& cur) { this->append(cur); });
	}

	inline const IterationVector& getIterationVector() const { return iterVec; }

	// Insert/appends a new AffineFunction to this system
	void insert(const iterator& pos, const AffineFunction& af);
	inline void append(const AffineFunction& af) { insert(end(), af); }

	// Insert/Append a new affine function taking the coefficients 
	template <class VectTy>
	inline void insert(const iterator& pos, const VectTy& coeffs) {
		insert(pos, AffineFunction(iterVec, coeffs) );
	}

	template <class VectTy>
	inline void append(const VectTy& coeffs) { insert(end(), coeffs); }

	// Removes rows from this affine system
	inline void remove(const iterator& iter) { funcs.erase( iter.get() ); }
	inline void remove(size_t pos) { funcs.erase( funcs.begin() + pos ); }
	
	inline void clear() { funcs.clear(); }

	template <class MatTy>
	void set(const MatTy& coeffs) { 
		// Clear the current matrix of coefficients 
		clear();
		for_each(coeffs, [&](const typename MatTy::value_type& cur) { append(cur); });
	}

	inline size_t size() const { return funcs.size(); }
	inline bool empty() const { return funcs.empty(); }

	inline iterator begin() { return iterator(funcs.begin(), funcs.end()); }
	inline iterator end() { return iterator(funcs.end(), funcs.end()); }

	inline const_iterator begin() const { 
		return const_iterator(funcs.cbegin(), funcs.cend()); 
	}
	inline const_iterator end() const {
		return const_iterator(funcs.cend(), funcs.cend()); 
	}

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
		const poly::AffineSystem&		access 
	) : expr(expr), type(type), usage(usage), 
		access( std::make_shared<poly::AffineSystem>(access) ) { }

	AccessInfo(const AccessInfo& other) : 
		expr(other.expr), type(other.type), usage(other.usage), 
		access( std::make_shared<AffineSystem>(*other.access) ) { }

	// Copy constructor with base (iterator vector) change 
	AccessInfo( const IterationVector& iterVec, const AccessInfo& other) : 
		expr(other.expr), type(other.type), usage(other.usage), 
		access( std::make_shared<AffineSystem>(iterVec, *other.access) ) { } 

	// Getters for expr/type and usage
	inline const core::ExpressionAddress& getExpr() const { return expr; }
	inline const Ref::RefType& getRefType() const { return type; }
	inline const Ref::UseType& getUsage() const { return usage; }

	// Getters/Setters for access functions 
	inline AffineSystem& getAccess() { return *access; }
	inline const AffineSystem& getAccess() const { return *access; }

	// implementing the printable interface
	std::ostream& printTo(std::ostream& out) const;
};

typedef std::vector<AccessInfo> AccessList;

/**************************************************************************************************
 * Stmt: this class assembles together the informations which are utilized to represent a
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
		  const AccessList& 			access = AccessList() 
		) 
	: id(id), addr(addr), dom(dom), schedule(schedule), access(access) { }

	Stmt( const IterationVector& iterVec, const Stmt& other ) 
	: 	id(other.id), 
		addr(other.addr), 
		dom(iterVec, other.dom), 
		schedule(iterVec, other.schedule) 
	{
		for_each(other.access, [&](const AccessInfo& cur) { access.push_back(AccessInfo(iterVec, cur)); });
	}

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

	// Getters/Setters for access list
	inline const AccessList& getAccess() const { return access; }
	inline AccessList& getAccess() { return access; }

	// Accessories for iterating through accesses of this statement (read/write)
	inline AccessIterator access_begin() { return access.begin(); }
	inline AccessIterator access_end() { return access.end(); }

	// Accessories for iterating through accesses of this statement (write only)
	inline ConstAccessIterator access_begin() const { return access.cbegin(); }
	inline ConstAccessIterator access_end() const { return access.cend(); }

	std::ostream& printTo(std::ostream& out) const;
};

//*************************************************************************************************
// Scop: This class is the entry point for any polyhedral model analysis / transformations. The
// purpose is to fully represent all the information of a polyhedral static control region (SCOP).
// Copies of this class can be created so that transformations to the model can be applied without
// changing other instances of this SCop. 
//
// By default a Scop object is associated to a polyhedral region using the ScopRegion annotation.
// When a transformation needs to be performed a deep copy of the Scop object is created and
// transformations are applied to it. 
//*************************************************************************************************
typedef std::shared_ptr<Stmt> StmtPtr;

struct Scop : public utils::Printable {

	typedef std::vector<StmtPtr> StmtVect;
	typedef StmtVect::iterator iterator;
	typedef StmtVect::const_iterator const_iterator;

	Scop(const IterationVector& iterVec, const StmtVect& stmts = StmtVect()) : 
		iterVec(iterVec), sched_dim(0) 
	{
		// rewrite all the access functions in terms of the new iteration vector
		for_each(stmts, [&] (const StmtPtr& stmt) { 
				this->push_back( Stmt(this->iterVec, *stmt) );
			});
	}

	// Copy constructor builds a deep copy of this SCoP. 
	Scop(const Scop& other) : iterVec(other.iterVec), sched_dim(other.sched_dim) {
		for_each(other.stmts, [&] (const StmtPtr& stmt) { this->push_back( *stmt ); });
	}

	std::ostream& printTo(std::ostream& out) const;

	// Adds a stmt to this scop. 
	void push_back( const Stmt& stmt );

	inline const IterationVector& getIterationVector() const { return iterVec; }
	inline IterationVector& getIterationVector() { return iterVec; }

	inline StmtVect getStmts() const { return stmts; }

	// Get iterators thorugh the statements contained in this SCoP
	inline iterator begin() { return stmts.begin(); }
	inline iterator end() { return stmts.end(); }

	inline const_iterator begin() const { return stmts.begin(); }
	inline const_iterator end() const { return stmts.end(); }

	// Access statements based on their ID
	inline const Stmt& operator[](size_t pos) const { return *stmts[pos]; }
	inline Stmt& operator[](size_t pos) { return *stmts[pos]; }

	inline size_t size() const { return stmts.size(); }
	inline const size_t& schedDim() const { return sched_dim; }
	inline size_t& schedDim() { return sched_dim; }

	size_t nestingLevel() const;

	/**
	 * Produces IR code from this SCoP. 
	 */
	core::NodePtr toIR(core::NodeManager& mgr) const;
	
	template <class Ctx> 
	poly::MapPtr<Ctx> getSchedule(Ctx& ctx) const;

	/**
	 * Computes analysis information for this SCoP
	 */
	template <class Ctx> 
	poly::MapPtr<Ctx> computeDeps(Ctx& ctx, const unsigned& d = 
			analysis::dep::RAW | analysis::dep::WAR | analysis::dep::WAW) const;


	bool isParallel() const;

	core::NodePtr optimizeSchedule(core::NodeManager& mgr);

private:

	IterationVector 	iterVec;
	StmtVect 			stmts;
	size_t				sched_dim;
};

/**
 * Converts a SCoP based on a specific iteration vector to a different base which is compatible 
 * i.e. it contains at least the same elements as the original iteration vector but it can contain
 * new parameters or iterators which will be automatically set to 0
 */
// Scop toBase(const Scop& s, const IterationVector& iterVec);


} // end poly namespace
} // end analysis namespace
} // end insieme namespace 




