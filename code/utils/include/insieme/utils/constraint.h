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

#include <memory>
#include <vector>

#include "boost/operators.hpp"
#include "boost/mpl/or.hpp"
#include <boost/type_traits.hpp>
#include "boost/utility/enable_if.hpp"
#include "insieme/utils/printable.h"

namespace insieme {
namespace utils {


// Generic function which returns the constant value of a generic formula (either Formula or 
// AffineFunctions). If func is not constant than an exception is expected to be thrown 
template <class FuncTy>
int asConstant(const FuncTy& func);

/**************************************************************************************************
 * Define the possible type of expressions: 
 * 		EQ -> f(x) == 0, NE -> f(x) != 0, GT -> f(x)  > 0
 * 		LT -> f(x)  < 0, GE -> f(x) >= 0, LE -> f(x) <= 0
 *
 * 		Usually underlying libraries require all the constraints to be in a specific for, i.e. f(x)
 * 		>= 0, but newer libraries like ISL allows for representation of more complex relationships,
 * 		therefore we keep at this stage the constraints in this form and let the backend deal with
 * 		the representation in the chosen library. 
 *************************************************************************************************/
enum ConstraintType { GT, LT, EQ, NE, GE, LE };	

/******************************************************************************************************
 * A Constraint is a linear affine expression limiting the polyhedron. A set of constraints will
 * define an iteration domain which is our polyhedron. A constraint is usually represented as an
 * inequality, i.e. f(x) <= 0, however we allow here for a more general representation allowing any
 * sort of constraint (==, !=, <, >, <= and >=) to be represented. 
 *****************************************************************************************************/
template <typename FuncTy>
struct Constraint : public utils::Printable, 
	public boost::equality_comparable<Constraint<FuncTy>>,
	public boost::less_than_comparable<Constraint<FuncTy>> 
{

	Constraint(const FuncTy& func, const ConstraintType& type = ConstraintType::GE) : 
		func(func), type(type) { }

	inline ConstraintType getType() const { return type; }

	inline const FuncTy& getFunction() const { return func; }

	bool operator==(const Constraint<FuncTy>& other) const {
		return func==other.func && type==other.type;
	}

	std::ostream& printTo(std::ostream& out) const { 
		out << func << " ";
		switch(type) {
		case EQ: out << "==";  break;	case NE: out << "!=";  break; 	case GT: out << ">";   break;
		case LT: out << "<";   break;	case GE: out << ">=";  break;	case LE: out << "<=";  break;
		}
		return out << " 0";
	}

	inline bool operator<(const Constraint<FuncTy>& other) const {
		if (func==other.func) {	return type < other.type; }
		return func < other.func; 
	}

	// Return whether the result of this constraint is true.
	inline bool isTrue() const {
		if (!isEvaluatable()) { throw "ERROR: not evaluatable!"; }
	
		int val = asConstant(func);

		return (val == 0 && (type==EQ || type==GE || type==LE)) || 
			   (val < 0 && (type==LE || type==LT || type==NE)) ||
			   (val > 0 && (type==GE || type==GT || type==NE));
	}

	// Return true whether the underlying function is evaluatable which means the formula
	// doesn't depend on any variable 
	inline bool isEvaluatable() const {
		return func.isConstant();
	}

private:
	const FuncTy 			func;
	const ConstraintType 	type;
};

/******************************************************************************************************
 * ConstraintCombiner: The constraint combiner has the task to combine multiple constraints into 
 * conjunctions (AND) or disjunctions (OR) of constraints which can be either in positive form of 
 * negated. Because arbitrary nested structures are possible, we built a binary tree containing 
 * constraints. 
 *****************************************************************************************************/
 
// Forward declaration for the Constraint combiner and its subclasses 
template <typename FuncTy>
class ConstraintCombiner;

template <typename FuncTy>
struct ConstraintCombinerPtr : public std::shared_ptr<ConstraintCombiner<FuncTy>> {

	typedef FuncTy func_type;

	ConstraintCombinerPtr() { }
	ConstraintCombinerPtr(const std::shared_ptr<ConstraintCombiner<FuncTy>>& cons) : 
		std::shared_ptr<ConstraintCombiner<FuncTy>>( cons ) { }
};

template <typename FuncTy>
class RawConstraintCombiner;

template <typename FuncTy>
class NegatedConstraintCombiner;

template <class FuncTy>
struct BinaryConstraintCombiner ;

/**************************************************************************************************
 * Visitor class used to visit a combination of constraints. Because constraints are combined
 * together in a composite (tree like) structure, it is therefore easier to visit the structure by
 * means of a visitor. 
 *
 * The implementation of the visitor is based on double-dispatch. 
 *************************************************************************************************/
template <class FuncTy>
struct ConstraintVisitor {
	// Visits a raw node (which contains a raw constraint)
	virtual void visit(const RawConstraintCombiner<FuncTy>& rcc) = 0;
	// Visits a negated constraints 
	virtual void visit(const NegatedConstraintCombiner<FuncTy>& ucc) = 0; 
	// Visits a combination of constraints which can either be a conjunction or a disjunction 
	virtual void visit(const BinaryConstraintCombiner<FuncTy>& bcc) = 0;
};

template <class FuncTy>
struct RecConstraintVisitor : public ConstraintVisitor<FuncTy> {

	RecConstraintVisitor(bool postOrder=false) : postOrder(postOrder) { }

	// Visits a raw node (which contains a raw constraint)
	virtual void visit(const RawConstraintCombiner<FuncTy>& rcc) { }
	
	// Visits a negated constraints 
	virtual void visit(const NegatedConstraintCombiner<FuncTy>& ucc) {
		ucc.getSubConstraint()->accept(*this);
	}

	// Visits a combination of constraints which can either be a conjunction or a disjunction 
	virtual void visit(const BinaryConstraintCombiner<FuncTy>& bcc) {
		if (postOrder) { bcc.getRHS()->accept(*this); }
		bcc.getLHS()->accept(*this); 
		if (!postOrder) { bcc.getRHS()->accept(*this); }
	}

private:
	bool postOrder;
};

namespace {
// Utility visitor for printing a constraint 
template <class FuncTy>
struct ConstraintPrinter : public RecConstraintVisitor<FuncTy> {
	
	std::ostream& out;

	ConstraintPrinter(std::ostream& out) : out(out) { }

	void visit(const RawConstraintCombiner<FuncTy>& rcc) { out << "(" << rcc.getConstraint() << ")"; }

	virtual void visit(const NegatedConstraintCombiner<FuncTy>& ucc) {
		out << "NOT"; ucc.getSubConstraint()->accept(*this);
	}

	virtual void visit(const BinaryConstraintCombiner<FuncTy>& bcc) {
		out << "(";
		bcc.getLHS()->accept(*this);
		out << (bcc.isConjunction() ? " AND " : " OR ");
		bcc.getRHS()->accept(*this);
		out << ")";
	}

};


} // end anonymous namespace 

/**************************************************************************************************
 * This class has the purpose to create conjunctions and/or disjunctions of constraints. This allows
 * to represent the domain spawned by control operations with a composed conditional expression
 *************************************************************************************************/
template <typename FuncTy>
struct ConstraintCombiner : public utils::Printable {
	// implements a simple double dispatch visitor for the Composite  
	virtual void accept(ConstraintVisitor<FuncTy>& v) const = 0; 
	
	std::ostream& printTo(std::ostream& out) const {
		ConstraintPrinter<FuncTy> vis(out);
		accept( vis );
		return out;
	}

	// Check whether 2 constraints are the same 
	virtual bool operator==(const ConstraintCombiner<FuncTy>& other) const = 0;
};

/**************************************************************************************************
 * This class is a wrapper for a plain Constraint. Utilized to combine constraints in a composite
 * like structure.
 *************************************************************************************************/
template <typename FuncTy>
class RawConstraintCombiner : public ConstraintCombiner<FuncTy> {
	Constraint<FuncTy> constraint; 
public:
	RawConstraintCombiner(const Constraint<FuncTy>& constraint) : constraint(constraint) { }
	
	// Returns the constraint embodied in this wrapper class
	inline const Constraint<FuncTy>& getConstraint() const { return constraint; }
	
	void accept(ConstraintVisitor<FuncTy>& v) const { v.visit(*this); }

	virtual bool operator==(const ConstraintCombiner<FuncTy>& other) const {
		if (const RawConstraintCombiner<FuncTy>* raw = dynamic_cast<const RawConstraintCombiner<FuncTy>*>(&other))
			return constraint == raw->constraint;
		return false;
	}
};

/**************************************************************************************************
 * This class represents the negation of a constraint. 
 *************************************************************************************************/
template <typename FuncTy>
class NegatedConstraintCombiner : public ConstraintCombiner<FuncTy> {
	ConstraintCombinerPtr<FuncTy> subComb;
public:
	NegatedConstraintCombiner(const ConstraintCombinerPtr<FuncTy>& comb) 
		: subComb( comb ) { }

	// Returns the negated constraint 
	inline const ConstraintCombinerPtr<FuncTy>& getSubConstraint() const { 
		return subComb; 
	}

	void accept(ConstraintVisitor<FuncTy>& v) const { v.visit(*this); }

	virtual bool operator==(const ConstraintCombiner<FuncTy>& other) const {
		if (const NegatedConstraintCombiner<FuncTy>* neg = dynamic_cast<const NegatedConstraintCombiner<FuncTy>*>(&other))
			return *subComb == *neg->subComb;
		return false;
	}

};

/**************************************************************************************************
 * This class represent the combination of two constraints which can be either a combined through a
 * AND or a OR operator. 
 *************************************************************************************************/
template <class FuncTy>
struct BinaryConstraintCombiner : public ConstraintCombiner<FuncTy> {
	
	enum Type { AND, OR };

	BinaryConstraintCombiner(
			const Type& type, 
			const ConstraintCombinerPtr<FuncTy>& lhs, 
			const ConstraintCombinerPtr<FuncTy>& rhs
	) : 
		type(type), lhs(lhs), rhs(rhs) { }

	void accept(ConstraintVisitor<FuncTy>& v) const { v.visit(*this); }

	inline const ConstraintCombinerPtr<FuncTy>& getLHS() const { return lhs; }
	inline const ConstraintCombinerPtr<FuncTy>& getRHS() const { return rhs; }

	// Return the type of the constraint
	inline const Type& getType() const { return type; }

	inline bool isConjunction() const { return type == AND; }
	inline bool isDisjunction() const { return type == OR; }

	virtual bool operator==(const ConstraintCombiner<FuncTy>& other) const {
		if (const BinaryConstraintCombiner<FuncTy>* bin = dynamic_cast<const BinaryConstraintCombiner<FuncTy>*>(&other))
			return type == bin->type && *lhs == *bin->lhs && *rhs == *bin->rhs;
		return false;
	}

private:
	Type type;
	ConstraintCombinerPtr<FuncTy> lhs, rhs;
};

namespace {

template <class... All>
class Combiner;

/**************************************************************************************************
 * Combiner class takes a list of constraints and assembles them together either in a conjunction or
 * a disjunction (depending on the provided type) and returns a pointer to the combiner containing
 * the constraints 
 *************************************************************************************************/
template <typename FuncTy, typename... Tail>
struct Combiner<ConstraintCombinerPtr<FuncTy>, Tail...> {

	static ConstraintCombinerPtr<FuncTy>
	make(const typename BinaryConstraintCombiner<FuncTy>::Type& type, 
		const ConstraintCombinerPtr<FuncTy>& head, const Tail&... args) 
	{
		return ConstraintCombinerPtr<FuncTy>( 
			std::make_shared<BinaryConstraintCombiner<FuncTy>>( 
				type, head, Combiner<Tail...>::make(type, args...) 
			)
		);
	}
};

/** 
 * This specialization represent the where only 1 constraint is remaining
 */
template <class FuncTy>
struct Combiner<ConstraintCombinerPtr<FuncTy>> {

	static ConstraintCombinerPtr<FuncTy> make(
			const typename BinaryConstraintCombiner<FuncTy>::Type& type, 
			const ConstraintCombinerPtr<FuncTy>& head) 
	{ 
		return head; 
	}

};

} // end anonymous namespace 

template <typename FuncTy, typename ...All>
ConstraintCombinerPtr<FuncTy> makeConjunction(const All& ... args) { 
	return Combiner<All...>::make(BinaryConstraintCombiner<FuncTy>::AND, args...); 
}

template <typename FuncTy, typename ...All>
ConstraintCombinerPtr<FuncTy> makeDisjunction(const All&... args) { 
	return Combiner<All...>::make(BinaryConstraintCombiner<FuncTy>::OR, args...); 
}


template <typename FuncTy>
ConstraintCombinerPtr<FuncTy> makeCombiner(const Constraint<FuncTy>& c) {
	return ConstraintCombinerPtr<FuncTy>(std::make_shared<RawConstraintCombiner<FuncTy>>(c));
}

template <typename FuncTy>
ConstraintCombinerPtr<FuncTy> makeCombiner(const ConstraintCombinerPtr<FuncTy>& cc) { return cc; }

//==== Operator definitions for Constraint =========================================================

// Redefinition of ~ operator with the semantics of NOT
template <class FuncTy, template <typename> class C>
typename boost::enable_if<
	boost::mpl::or_<boost::is_same<C<FuncTy>, Constraint<FuncTy>>, boost::is_same<C<FuncTy>,ConstraintCombinerPtr<FuncTy>>
>, ConstraintCombinerPtr<FuncTy>>::type not_(const C<FuncTy>& c) { 
	return ConstraintCombinerPtr<FuncTy>(std::make_shared<NegatedConstraintCombiner<FuncTy>>(makeCombiner(c))); 
}

// Redefinition of && operarator with the semantics of AND 
template <class FuncTy, template <typename> class C1, template <typename> class C2>
typename boost::enable_if<
	boost::mpl::and_<
		boost::mpl::or_<boost::is_same<C1<FuncTy>,Constraint<FuncTy>>, boost::is_same<C1<FuncTy>,ConstraintCombinerPtr<FuncTy>>>,
		boost::mpl::or_<boost::is_same<C2<FuncTy>,Constraint<FuncTy>>, boost::is_same<C2<FuncTy>,ConstraintCombinerPtr<FuncTy>>>
	>, ConstraintCombinerPtr<FuncTy>>::type 
operator and(const C1<FuncTy>& lhs, const C2<FuncTy>& rhs) { 
	ConstraintCombinerPtr<FuncTy>&& lhsPtr = makeCombiner(lhs);
	ConstraintCombinerPtr<FuncTy>&& rhsPtr = makeCombiner(rhs);
	if (!lhsPtr) return rhsPtr;
	if (!rhsPtr) return lhsPtr;
	// FIXME: check whether the iteration vectors of lhs and rhs are compatible for the constraints
	// to be composed
	return makeConjunction<FuncTy>(lhsPtr, rhsPtr); 
}

// Redefinition of || operator with the semantics of OR 
template <class FuncTy, template <typename> class C1, template <typename> class C2>
typename boost::enable_if<
	boost::mpl::and_<
		boost::mpl::or_<boost::is_same<C1<FuncTy>,Constraint<FuncTy>>, boost::is_same<C1<FuncTy>,ConstraintCombinerPtr<FuncTy>>>,
		boost::mpl::or_<boost::is_same<C2<FuncTy>,Constraint<FuncTy>>, boost::is_same<C2<FuncTy>,ConstraintCombinerPtr<FuncTy>>>
	>, ConstraintCombinerPtr<FuncTy>>::type
operator or(const C1<FuncTy>& lhs, const C2<FuncTy>& rhs) { 
	ConstraintCombinerPtr<FuncTy> lhsPtr = makeCombiner(lhs);
	ConstraintCombinerPtr<FuncTy> rhsPtr = makeCombiner(rhs);
	if (!lhsPtr) return rhsPtr;
	if (!rhsPtr) return lhsPtr;
	// FIXME: check whether the iteration vectors of lhs and rhs are compatible for the constraints
	// to be composed
	return makeDisjunction<FuncTy>(lhsPtr, rhsPtr); 
}


namespace {

template <class FuncTy>
struct ConstraintNormalizer : public RecConstraintVisitor<FuncTy> {

	ConstraintCombinerPtr<FuncTy> curr;

	ConstraintNormalizer() { }

	void visit(const RawConstraintCombiner<FuncTy>& rcc) { 
		curr = normalize( rcc.getConstraint() ); 
	}

	void visit(const NegatedConstraintCombiner<FuncTy>& ucc) { 
		ucc.getSubConstraint()->accept(*this);
		assert(curr);

		if (std::shared_ptr<NegatedConstraintCombiner<FuncTy>> subCons =
			std::dynamic_pointer_cast<NegatedConstraintCombiner<FuncTy>>( curr )) 
		{ 
			curr = subCons->getSubConstraint(); 
			return;
		}
		curr = not_ ( curr );
	}

	void visit(const BinaryConstraintCombiner<FuncTy>& bcc) {
		bcc.getLHS()->accept(*this);
		assert(curr);
		ConstraintCombinerPtr<FuncTy> lhs = curr;
		bcc.getRHS()->accept(*this);
		assert(curr);
		ConstraintCombinerPtr<FuncTy> rhs = curr; 

		curr = bcc.getType() == BinaryConstraintCombiner<FuncTy>::OR ? 
			lhs or rhs : lhs and rhs;
	}

};

} // end anonymous namespace 

template <class FuncTy>
inline ConstraintCombinerPtr<FuncTy> normalize( const ConstraintCombinerPtr<FuncTy>& cons ) {
	ConstraintNormalizer<FuncTy> cnv;
	cons->accept(cnv);
	return cnv.curr;
}

namespace {

template <class SrcTy, class TrgTy>
struct ConstraintConverter : public RecConstraintVisitor<SrcTy> {

	ConstraintCombinerPtr<TrgTy> curr;

	ConstraintConverter() { }

	void visit(const RawConstraintCombiner<SrcTy>& rcc) { 
		curr = makeCombiner(Constraint<TrgTy>( 
					static_cast<TrgTy>(rcc.getConstraint().getFunction()), 
					rcc.getConstraint().getType() 
				));  
	}

	void visit(const NegatedConstraintCombiner<SrcTy>& ucc) { 
		ucc.getSubConstraint()->accept(*this);
		assert(curr);
		curr = not_( curr );
	}

	void visit(const BinaryConstraintCombiner<SrcTy>& bcc) {
		bcc.getLHS()->accept(*this);
		assert(curr);
		ConstraintCombinerPtr<TrgTy> lhs = curr;
		bcc.getRHS()->accept(*this);
		assert(curr);
		ConstraintCombinerPtr<TrgTy> rhs = curr; 

		curr = bcc.getType() == BinaryConstraintCombiner<SrcTy>::OR ? 
			lhs or rhs : lhs and rhs;
	}

};

} // end anonymous namespace 

template <class SrcTy, class TrgTy>
inline ConstraintCombinerPtr<TrgTy> castTo( 
		const ConstraintCombinerPtr<SrcTy>& cons, 
		typename boost::enable_if<boost::is_convertible<SrcTy,TrgTy>>::type* dummy = 0) 
{
	ConstraintConverter<SrcTy, TrgTy> cnv;
	cons->accept(cnv);
	return cnv.curr;
}

/**
 * Picewise represent a generic class used to represent piesewise polynomials or functions.
 * It is represented by a set of pieces each of them containing a constraint which defines 
 * the range for which a piece is defined and its value for that range. 
 *
 * Carefull that no checks are conducted to make sure that the pieces are disjoints so make
 * sure that this is the case when the piecewise is constructed otherwise you may end up with
 * overlapping pieces and obtain undesired results 
 */
template <class FuncTy>
struct Piecewise : Printable {

	typedef Constraint<FuncTy> 			  		Predicate;
	typedef ConstraintCombinerPtr<FuncTy>  		PredicatePtr;
	typedef ConstraintType 	   			  		PredicateType;

	typedef std::pair<PredicatePtr, FuncTy> 	Piece;

	typedef std::vector<Piece> 					Pieces;
	typedef typename Pieces::iterator			iterator;
	typedef typename Pieces::const_iterator		const_iterator;

	Piecewise( const Pieces& pieces ) : pieces(pieces) { }

	// Build a piecewise containing only 1 piece
	Piecewise( const PredicatePtr& pred, const FuncTy& trueVal, const FuncTy& falseVal = FuncTy()) 
		: pieces( { Piece(normalize(pred), trueVal), 
					Piece(normalize(not_(pred)), falseVal) 
				  } ) { }

	std::ostream& printTo(std::ostream& out) const {
		return out << join("; ", pieces, [&](std::ostream& out, const Piece& cur) {
			out << cur.second << " -> if " << *cur.first;
		});
	}

	inline iterator begin() { return pieces.begin(); }
	inline iterator end() { return pieces.end(); }

	inline const_iterator begin() const { return pieces.begin(); }
	inline const_iterator end() const { return pieces.end(); }

	inline size_t size() const { return pieces.size(); }

	inline bool empty() const { return pieces.empty(); }

private:
	Pieces pieces;
};

} // end utils namespace
} // end insieme namespace
