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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/core/ast_node.h"
#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"
#include "boost/mpl/or.hpp"

namespace insieme {
namespace analysis {
namespace poly {

/******************************************************************************************************
 * A constraint is a linear affine expression limiting the polyhedron. A set of constraints will
 * define an iteration domain which is our polyhedron. A constraint is usually represented as an
 * inequality, i.e. f(x) <= 0, however we allow here for a more general representation allowing any
 * sort of constraint (==, !=, <, >, <= and >=) to be represented. 
 *****************************************************************************************************/
struct Constraint : public utils::Printable, 
	public boost::equality_comparable<Constraint>,
	public boost::less_than_comparable<Constraint> 
{
	/**
	 * Define the possible type of expressions: 
	 * 		EQ -> f(x) == 0, NE -> f(x) != 0, GT -> f(x)  > 0
	 * 		LT -> f(x)  < 0, GE -> f(x) >= 0, LE -> f(x) <= 0
	 *
	 * 		Usually underlying libraries require all the constraints to be in a specific for, i.e. f(x)
	 * 		>= 0, but newer libraries like ISL allows for representation of more complex relationships,
	 * 		therefore we keep at this stage the constraints in this form and let the backend deal with
	 * 		the representation in the chosen library. 
	 */
	 enum Type { GT, LT, EQ, NE, GE, LE };	

	Constraint(const AffineFunction& af, const Type& type) : af(af), type(type) { }

	inline Type getType() const { return type; }

	inline const AffineFunction& getAffineFunction() const { return af; }

	std::ostream& printTo(std::ostream& out) const; 

	bool operator==(const Constraint& other) const {
		return af==other.af && type==other.type;
	}

	bool operator<(const Constraint& other) const;

	Constraint 
	toBase(const IterationVector& iterVec, const IndexTransMap& idxMap = IndexTransMap()) const;

private:
	const AffineFunction af;
	const Type type;
};

/******************************************************************************************************
 * ConstraintCombiner: The constraint combiner has the task to combine multiple constraints into 
 * conjunctions (AND) or disjunctions (OR) of constraints which can be either in positive form of 
 * negated. Because arbitrary nested structures are possible, we built a binary tree containing 
 * constraints. 
 *****************************************************************************************************/
 
// Forward declaration for the Constraint combiner 
class ConstraintCombiner;

typedef std::shared_ptr<ConstraintCombiner> ConstraintCombinerPtr; 

// forward declaration for the Constraint visitor 
struct ConstraintVisitor; 

/**************************************************************************************************
 * This class has the purpose to create conjunctions and/or disjunctions of constraints. This allows
 * to represent the domain spawned by control operations with a composed conditional expression
 *************************************************************************************************/
struct ConstraintCombiner : public utils::Printable {
	// implements a simple double dispatch visitor for the Composite  
	virtual void accept(ConstraintVisitor& v) const = 0; 
	
	std::ostream& printTo(std::ostream& out) const;
};

/**************************************************************************************************
 * This class is a wrapper for a plain Constraint. Utilized to combine constraints in a composite
 * like structure.
 *************************************************************************************************/
class RawConstraintCombiner : public ConstraintCombiner {
	Constraint constraint; 
public:
	RawConstraintCombiner(const Constraint& constraint) : constraint(constraint) { }
	
	// Returns the constraint embodied in this wrapper class
	inline const Constraint& getConstraint() const { return constraint; }
	
	void accept(ConstraintVisitor& v) const;
};

/**************************************************************************************************
 * This class represents the negation of a constraint. 
 *************************************************************************************************/
class NegatedConstraintCombiner : public ConstraintCombiner {
	ConstraintCombinerPtr subComb;
public:
	NegatedConstraintCombiner(const ConstraintCombinerPtr& comb) : subComb( comb ) { }

	// Returns the negated constraint 
	inline const ConstraintCombinerPtr& getSubConstraint() const { return subComb; }

	void accept(ConstraintVisitor& v) const;
};

/**************************************************************************************************
 * This class represent the combination of two constraints which can be either a combined through a
 * AND or a OR operator. 
 *************************************************************************************************/
struct BinaryConstraintCombiner : public ConstraintCombiner {
	
	enum Type { AND, OR };

	BinaryConstraintCombiner(const Type& type, const ConstraintCombinerPtr& lhs, 
			const ConstraintCombinerPtr& rhs) : 
		ConstraintCombiner(), type(type), lhs(lhs), rhs(rhs) { }

	void accept(ConstraintVisitor& v) const;

	inline const ConstraintCombinerPtr& getLHS() const { return lhs; }
	inline const ConstraintCombinerPtr& getRHS() const { return rhs; }

	// Return the type of the constraint
	inline const Type& getType() const { return type; }

	inline bool isConjunction() const { return type == AND; }
	inline bool isDisjunction() const { return type == OR; }

private:
	Type type;
	ConstraintCombinerPtr lhs, rhs;
};

/**************************************************************************************************
 * Visitor class used to visit a combination of constraints. Because constraints are combined
 * together in a composite (tree like) structure, it is therefore easier to visit the structure by
 * means of a visitor. 
 *
 * The implementation of the visitor is based on double-dispatch. 
 *************************************************************************************************/
struct ConstraintVisitor {
	
	// Visits a raw node (which contains a raw constraint)
	virtual void visit(const RawConstraintCombiner& rcc) { }
	
	// Visits a negated constraints 
	virtual void visit(const NegatedConstraintCombiner& ucc) {
		ucc.getSubConstraint()->accept(*this);
	}

	// Visits a combination of constraints which can either be a conjunction or a disjunction 
	virtual void visit(const BinaryConstraintCombiner& bcc) {
		bcc.getLHS()->accept(*this); bcc.getRHS()->accept(*this);
	}
};

namespace {

template <class... All>
class Combiner;

/**************************************************************************************************
 * Combiner class takes a list of constraints and assembles them together either in a conjunction or
 * a disjunction (depending on the provided type) and returns a pointer to the combiner containing
 * the constraints 
 *************************************************************************************************/
template <class Head, class... Tail>
struct Combiner<Head, Tail...> {
	static ConstraintCombinerPtr 
	make(const BinaryConstraintCombiner::Type& type, const Head& head, const Tail&... args) {
		return std::make_shared<BinaryConstraintCombiner>( type, head, 
				Combiner<Tail...>::make(type, args...) 
			);
	}
};

/** 
 * This specialization represent the where only 1 constraint is remaining
 */
template <class Head>
struct Combiner<Head> {
	static ConstraintCombinerPtr make(const BinaryConstraintCombiner::Type& type, const Head& head) { 
		return head; 
	}
};

} // end anonymous namespace 

template <class ...All>
ConstraintCombinerPtr makeConjunction(const All& ... args) { 
	return Combiner<All...>::make(BinaryConstraintCombiner::AND, args...); 
}

template <class ...All>
ConstraintCombinerPtr makeDisjunction(const All&... args) { 
	return Combiner<All...>::make(BinaryConstraintCombiner::OR, args...); 
}

ConstraintCombinerPtr makeCombiner(const Constraint& c);
ConstraintCombinerPtr makeCombiner(const ConstraintCombinerPtr& cc);

// Makes a copy of the constraint cc changing the base vector to the iteration vector trgVec. 
ConstraintCombinerPtr cloneConstraint(const IterationVector& trgVec, const ConstraintCombinerPtr& cc);

// We normalize the constraint, usually required for libraries. 
// Equality constraints remains the same while inequalities must be rewritten to be GE (>=)
ConstraintCombinerPtr normalize(const Constraint& c);


//==== Operator definitions for Constraint =========================================================

// Redefinition of ~ operator with the semantics of NOT
template <class C>
typename boost::enable_if<
	boost::mpl::or_<boost::is_same<C,Constraint>, boost::is_same<C,ConstraintCombinerPtr>
>, ConstraintCombinerPtr>::type not_(const C& c) { 
	return std::make_shared<NegatedConstraintCombiner>(makeCombiner(c)); 
}

// Redefinition of && operarator with the semantics of AND 
template <class C1, class C2>
typename boost::enable_if<
	boost::mpl::and_<
		boost::mpl::or_<boost::is_same<C1,Constraint>, boost::is_same<C1,ConstraintCombinerPtr>>,
		boost::mpl::or_<boost::is_same<C2,Constraint>, boost::is_same<C2,ConstraintCombinerPtr>>
	>, ConstraintCombinerPtr>::type 
operator and(const C1& lhs, const C2& rhs) { 
	ConstraintCombinerPtr lhsPtr = makeCombiner(lhs);
	ConstraintCombinerPtr rhsPtr = makeCombiner(rhs);
	if (!lhsPtr) return rhsPtr;
	if (!rhsPtr) return lhsPtr;
	return makeConjunction(makeCombiner(lhs), makeCombiner(rhs)); 
}

// Redefinition of || operator with the semantics of OR 
template <class C1, class C2>
typename boost::enable_if<
	boost::mpl::and_<
		boost::mpl::or_<boost::is_same<C1,Constraint>, boost::is_same<C1,ConstraintCombinerPtr>>,
		boost::mpl::or_<boost::is_same<C2,Constraint>, boost::is_same<C2,ConstraintCombinerPtr>>
	>, ConstraintCombinerPtr>::type
operator or(const C1& lhs, const C2& rhs) { 
	ConstraintCombinerPtr lhsPtr = makeCombiner(lhs);
	ConstraintCombinerPtr rhsPtr = makeCombiner(rhs);
	if (!lhsPtr) return rhsPtr;
	if (!rhsPtr) return lhsPtr;
	return makeDisjunction(makeCombiner(lhs), makeCombiner(rhs)); 
}

// Defines a list of constraints stored in a vector
typedef std::vector<Constraint> ConstraintList;

} // end poly namespace
} // end analysis namespace
} // end insieme namespace 

