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
#include <cassert>
#include <stdexcept>
#include <iostream>

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

// Returns the constrait type which correspond to the logic negation of a given constraint type
inline ConstraintType getLogicNegation(const ConstraintType& c) {

	switch(c) {
	case ConstraintType::EQ: return ConstraintType::NE;
	case ConstraintType::NE: return ConstraintType::EQ;

	case ConstraintType::LT: return ConstraintType::GE;
	case ConstraintType::LE: return ConstraintType::GT;

	case ConstraintType::GT: return ConstraintType::LE;
	case ConstraintType::GE: return ConstraintType::LT;

	default: assert(false && "Constraint type not supported");
	}
	return ConstraintType::GT;	// some return is required!
}

/******************************************************************************************************
 * A Constraint is a linear affine expression limiting the polyhedron. A set of constraints will
 * define an iteration domain which is our polyhedron. A constraint is usually represented as an
 * inequality, i.e. f(x) <= 0, however we allow here for a more general representation allowing any
 * sort of constraint (==, !=, <, >, <= and >=) to be represented. 
 *****************************************************************************************************/
template <typename FuncTy>
struct Constraint : 
	public utils::Printable, 
	public boost::equality_comparable<Constraint<FuncTy>>,
	public boost::less_than_comparable<Constraint<FuncTy>> 
{

	/** 
	 * R-value move constructor 
	 */
	Constraint(Constraint<FuncTy>&& cons) : 
		func(std::move(cons.func)), 
		type(cons.type) { }

	/**
	 * Copy constructor 
	 */
	Constraint(const Constraint<FuncTy>& cons) : func(cons.func), type(cons.type) { }

	/** 
	 * R-Value move semantics constructor 
	 */
	Constraint(FuncTy&& func, ConstraintType type = ConstraintType::GE) :
		func(std::move(func)), type(type) { }

	/**
	 * Standard costructor 
	 */
	Constraint(const FuncTy& func, const ConstraintType& type = ConstraintType::GE) : 
		func(func), type(type) { }

	/** 
	 * Return the type of this constraint 
	 */
	inline ConstraintType getType() const { return type; }

	inline const FuncTy& getFunction() const { return func; }

	inline bool operator==(const Constraint<FuncTy>& other) const {
		return func==other.func && type==other.type;
	}

	inline std::ostream& printTo(std::ostream& out) const { 
		out << func << " ";
		switch(type) {
		case EQ: out << "==";  break;	case NE: out << "!=";  break; 	case GT: out << ">";   break;
		case LT: out << "<";   break;	case GE: out << ">=";  break;	case LE: out << "<=";  break;
		}
		return out << " 0";
	}

	inline bool operator<(const Constraint<FuncTy>& other) const {
		return func < other.func || (func == other.func && type < other.type);
	}

	// Return whether the result of this constraint is true.
	inline bool isTrue() const {
		if (!isEvaluable()) { throw std::logic_error("ERROR: not evaluatable!"); /* FIXME: introduce an exception for this */ }
	
		int val = asConstant(func);

		return (val == 0 && (type==EQ || type==GE || type==LE)) || 
			   (val < 0 && (type==LE || type==LT || type==NE)) ||
			   (val > 0 && (type==GE || type==GT || type==NE));
	}

	inline operator bool() const { return isTrue(); }

	// Return true whether the underlying function is evaluatable which means the formula
	// doesn't depend on any variable 
	inline bool isEvaluable() const { return func.isConstant(); }

private:
	const FuncTy 			func;
	const ConstraintType 	type;
};


template <>
inline bool Constraint<int>::isEvaluable() const { return true; }



/******************************************************************************************************
 * Combiner: The constraint combiner has the task to combine multiple constraints into 
 * conjunctions (AND) or disjunctions (OR) of constraints which can be either in positive form of 
 * negated. Because arbitrary nested structures are possible, we built a binary tree containing 
 * constraints. 
 *****************************************************************************************************/
 
// Forward declaration for the Constraint combiner and its subclasses 
template <typename FuncTy>
class Combiner;

enum CombinerType { CT_RAW, CT_NEG, CT_BIN };

template <typename FuncTy>
struct CombinerPtr : public std::shared_ptr<Combiner<FuncTy>> {

	typedef FuncTy func_type;
	
	/** 
	 * Empty constructor builds an empty combiner
	 */
	CombinerPtr() { }

	CombinerPtr(const std::shared_ptr<Combiner<FuncTy>>& cons) : 
		std::shared_ptr<Combiner<FuncTy>>( cons ) { }

	CombinerPtr(const CombinerPtr<FuncTy>& other) : 
		std::shared_ptr<Combiner<FuncTy>>( other ) { }

	CombinerPtr(CombinerPtr<FuncTy>&& other) :
		std::shared_ptr<Combiner<FuncTy>>( std::move(other) ) { }

	/** 
	 * Automatic conversion to bool of a combiner
	 */
	operator bool() const {
		return static_cast<bool>(static_cast<const std::shared_ptr<Combiner<FuncTy>>&>(*this));
	}

};


/**************************************************************************************************
 * This class has the purpose to create conjunctions and/or disjunctions of constraints. This allows
 * to represent the domain spawned by control operations with a composed conditional expression
 *************************************************************************************************/
template <typename FuncTy>
struct Combiner: public utils::Printable {
	
	std::ostream& printTo(std::ostream& out) const;

	// Check whether 2 constraints are the same 
	virtual bool operator==(const Combiner<FuncTy>& other) const = 0;

	// Check whether this combination of constraints is evaluatable
	virtual bool isEvaluable() const = 0;

	// Return true whether this combination of constraints is evaluatable to true
	virtual bool isTrue() const = 0;

	inline operator bool() const { return isTrue(); }
	
	virtual CombinerType getCombinerType() const = 0;

};



/**************************************************************************************************
 * This class is a wrapper for a plain Constraint. Utilized to combine constraints in a composite
 * like structure.
 *************************************************************************************************/
template <typename FuncTy>
class RawConstraint : public Combiner<FuncTy> {
	Constraint<FuncTy> constraint; 
public:

	/** 
	 * R-value move constructor 
	 */
	RawConstraint(Constraint<FuncTy>&& constraint) : 
		constraint(std::move(constraint)) { }

	/** 
	 * Copy constructor 
	 */
	RawConstraint(const Constraint<FuncTy>& constraint) : 
		constraint(constraint) { }

	// Returns the constraint embodied in this wrapper class
	inline const Constraint<FuncTy>& getConstraint() const { return constraint; }
	
	virtual bool operator==(const Combiner<FuncTy>& other) const {
		// Check trivial case
		if (this == &other) { return true; }

		if (const RawConstraint<FuncTy>* raw = dynamic_cast<const RawConstraint<FuncTy>*>(&other))
			return constraint == raw->constraint;

		return false;
	}

	inline bool isEvaluable() const { return constraint.isEvaluable(); }

	inline bool operator<(const RawConstraint<FuncTy>& other) const { 
		return constraint < other.constraint; 
	}

	inline bool isTrue() const { return constraint.isTrue(); }

	inline CombinerType getCombinerType() const { return CT_RAW; }
};

template <>
inline bool RawConstraint<int>::isEvaluable() const { return true; }

/**************************************************************************************************
 * This class represents the negation of a constraint. 
 *************************************************************************************************/
template <typename FuncTy>
class NegConstraint : public Combiner<FuncTy> {
	CombinerPtr<FuncTy> subComb;

public:
	NegConstraint(const CombinerPtr<FuncTy>& comb) : subComb( comb ) { }

	// Returns the negated constraint 
	inline const CombinerPtr<FuncTy>& getSubConstraint() const { 
		return subComb; 
	}

	virtual bool operator==(const Combiner<FuncTy>& other) const {
		// Check the trivial case
		if (this == &other) { return true; }

		if (const NegConstraint<FuncTy>* neg = 
				dynamic_cast<const NegConstraint<FuncTy>*>(&other))
		{
			return *subComb == *neg->subComb;
		}

		return false;
	}

	inline bool isEvaluable() const { return subComb->isEvaluable(); }

	inline bool isTrue() const { return !subComb->isTrue(); }

	inline CombinerType getCombinerType() const { return CT_NEG; }
};


/**************************************************************************************************
 * This class represent the combination of two constraints which can be either a combined through a
 * AND or a OR operator. 
 *************************************************************************************************/
template <class FuncTy>
struct BinConstraint : public Combiner<FuncTy> {
	
	enum Type { AND, OR };

	BinConstraint(const Type& type, const CombinerPtr<FuncTy>& lhs, const CombinerPtr<FuncTy>& rhs) : 
		type(type), lhs(lhs), rhs(rhs) { }

	inline const CombinerPtr<FuncTy>& getLHS() const { return lhs; }
	inline const CombinerPtr<FuncTy>& getRHS() const { return rhs; }

	// Return the type of the constraint
	inline const Type& getType() const { return type; }

	inline bool isConjunction() const { return type == AND; }
	inline bool isDisjunction() const { return type == OR; }

	virtual bool operator==(const Combiner<FuncTy>& other) const {
		// check trivial case
		if (this == &other) { return true; }

		if (const BinConstraint<FuncTy>* bin = dynamic_cast<const BinConstraint<FuncTy>*>(&other)) {
			return type == bin->type && *lhs == *bin->lhs && *rhs == *bin->rhs;
		}

		return false;
	}

	inline bool isEvaluable() const {
		return lhs->isEvaluable();
	}

	inline bool isTrue() const {
		return (lhs->isTrue() && type == OR) || 
			   (type == OR && rhs->isEvaluable() && rhs->isTrue()) || 
			   (lhs->isTrue() && rhs->isEvaluable() && rhs->isTrue());
	}

	inline CombinerType getCombinerType() const { return CT_BIN; }

private:
	Type type;
	CombinerPtr<FuncTy> lhs, rhs;
};




/**************************************************************************************************
 * Visitor class used to visit a combination of constraints. Because constraints are combined
 * together in a composite (tree like) structure, it is therefore easier to visit the structure by
 * means of a visitor. 
 *
 * The implementation of the visitor is based on double-dispatch. 
 *************************************************************************************************/
template <class FuncTy, class RetTy = void>
struct ConstraintVisitor {

	// Visits a raw node (which contains a raw constraint)
	virtual RetTy visitRawConstraint(const RawConstraint<FuncTy>& rcc) = 0;

	// Visits a negated constraints 
	virtual RetTy visitNegConstraint(const NegConstraint<FuncTy>& ucc) = 0;

	// Visits a combination of constraints which can either be a conjunction or a disjunction 
	virtual RetTy visitBinConstraint(const BinConstraint<FuncTy>& bcc) = 0;

	virtual RetTy visit(const Combiner<FuncTy>& cur) {
		// Dispatch to raw constraint
		if (const RawConstraint<FuncTy>* rc = dynamic_cast<const RawConstraint<FuncTy>*>(&cur)) {
			return visitRawConstraint(*rc);
		}
		
		if (const NegConstraint<FuncTy>* nc = dynamic_cast<const NegConstraint<FuncTy>*>(&cur)) {
			return visitNegConstraint(*nc);
		}

		if (const BinConstraint<FuncTy>* bc = dynamic_cast<const BinConstraint<FuncTy>*>(&cur)) {
			return visitBinConstraint(*bc);
		}
		//std::cout << cur;
		assert(false && "Constraint Combiner not supported");
		return RetTy();
	}

	virtual RetTy visit(const CombinerPtr<FuncTy>& comb) { return visit(*comb); }
};



template <class FuncTy, class RetTy>
struct RecConstraintVisitor : public ConstraintVisitor<FuncTy, RetTy> {

	RecConstraintVisitor(bool postOrder=false) : postOrder(postOrder) { }

	// Visits a raw node (which contains a raw constraint)
	virtual RetTy visitRawConstraint(const RawConstraint<FuncTy>& rcc) { return RetTy();}
	 
	// Visits a negated constraints 
	virtual RetTy visitNegConstraint(const NegConstraint<FuncTy>& ucc) {
		return this->visit(ucc.getSubConstraint());
	}

	// Visits a combination of constraints which can either be a conjunction or a disjunction 
	virtual RetTy visitBinConstraint(const BinConstraint<FuncTy>& bcc) {
		if (postOrder) { this->visit(bcc.getRHS()); }
		
		this->visit(bcc.getLHS()); 

		if (!postOrder) { this->visit(bcc.getRHS()); }

		return RetTy();
	}

private:
	bool postOrder;
};


namespace {
// Utility visitor for printing a constraint 
template <class FuncTy>
struct ConstraintPrinter : public RecConstraintVisitor<FuncTy, void> {
	
	std::ostream& out;

	ConstraintPrinter(std::ostream& out) : out(out) { }

	void visitRawConstraint(const RawConstraint<FuncTy>& rcc) { 
		out << "(" << rcc.getConstraint() << ")"; 
	}

	void visitNegConstraint(const NegConstraint<FuncTy>& ucc) {
		out << "!";
		this->visit(ucc.getSubConstraint());
	}

	void visitBinConstraint(const BinConstraint<FuncTy>& bcc) {
		out << "(";
		this->visit(bcc.getLHS());

		out << (bcc.isConjunction() ? " ^ " : " v ");
		this->visit(bcc.getRHS());
		
		out << ")";
	}

};


} // end anonymous namespace 

template <class FuncTy>
std::ostream& Combiner<FuncTy>::printTo(std::ostream& out) const {
	ConstraintPrinter<FuncTy>(out).visit(*this);
	return out;
}

namespace {

template <class... All>
class __combiner;

/**************************************************************************************************
 * Combiner class takes a list of constraints and assembles them together either in a conjunction or
 * a disjunction (depending on the provided type) and returns a pointer to the combiner containing
 * the constraints 
 *************************************************************************************************/
template <typename FuncTy, typename... Tail>
struct __combiner< CombinerPtr<FuncTy>, Tail... > {

	static CombinerPtr<FuncTy>
	make(const typename BinConstraint<FuncTy>::Type& type, 
		const CombinerPtr<FuncTy>& head, const Tail&... args) 
	{
		return CombinerPtr<FuncTy>( 
			std::make_shared<BinConstraint<FuncTy>>( 
				type, head, __combiner<Tail...>::make(type, args...) 
			)
		);
	}
};

/** 
 * This specialization represent the where only 1 constraint is remaining
 */
template <class FuncTy>
struct __combiner<CombinerPtr<FuncTy>> {

	static CombinerPtr<FuncTy> make(
			const typename BinConstraint<FuncTy>::Type& type, 
			const CombinerPtr<FuncTy>& head) 
	{ 
		return head; 
	}

};

} // end anonymous namespace 

template <typename FuncTy, typename ...All>
CombinerPtr<FuncTy> makeConjunction(const All& ... args) { 
	return __combiner<All...>::make(BinConstraint<FuncTy>::AND, args...); 
}

template <typename FuncTy, typename ...All>
CombinerPtr<FuncTy> makeDisjunction(const All&... args) { 
	return __combiner<All...>::make(BinConstraint<FuncTy>::OR, args...); 
}

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(const Constraint<FuncTy>& c) {
	return CombinerPtr<FuncTy>(std::make_shared<RawConstraint<FuncTy>>(c));
}

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(Constraint<FuncTy>&& c) {
	return CombinerPtr<FuncTy>(std::make_shared<RawConstraint<FuncTy>>(c));
}

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(const CombinerPtr<FuncTy>& cc) { return cc; }

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(const RawConstraint<FuncTy>& c) {
	return CombinerPtr<FuncTy>(std::make_shared<RawConstraint<FuncTy>>(c.getConstraint()));
}

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(const NegConstraint<FuncTy>& c) {
	return CombinerPtr<FuncTy>(std::make_shared<NegConstraint<FuncTy>>(c.getSubConstraint()));
}

template <typename FuncTy>
CombinerPtr<FuncTy> makeCombiner(const BinConstraint<FuncTy>& c) {
	return CombinerPtr<FuncTy>(std::make_shared<BinConstraint<FuncTy>>(c.getType(), c.getLHS(), c.getRHS()));
}

//==== Operator definitions for Constraint =========================================================

// Redefinition of ~ operator with the semantics of NOT
template <class FuncTy, template <typename> class C>
typename boost::enable_if<
	boost::mpl::or_<boost::is_same<C<FuncTy>, Constraint<FuncTy>>, boost::is_same<C<FuncTy>,CombinerPtr<FuncTy>>
>, CombinerPtr<FuncTy>>::type not_(const C<FuncTy>& c) { 
	return CombinerPtr<FuncTy>(std::make_shared<NegConstraint<FuncTy>>(makeCombiner(c))); 
}

// Redefinition of && operarator with the semantics of AND 
template <class FuncTy, template <typename> class C1, template <typename> class C2>
typename boost::enable_if<
	boost::mpl::and_<
		boost::mpl::or_<boost::is_same<C1<FuncTy>,Constraint<FuncTy>>, boost::is_same<C1<FuncTy>,CombinerPtr<FuncTy>>>,
		boost::mpl::or_<boost::is_same<C2<FuncTy>,Constraint<FuncTy>>, boost::is_same<C2<FuncTy>,CombinerPtr<FuncTy>>>
	>, CombinerPtr<FuncTy>>::type 
operator and(const C1<FuncTy>& lhs, const C2<FuncTy>& rhs) { 
	CombinerPtr<FuncTy>&& lhsPtr = makeCombiner(lhs);
	CombinerPtr<FuncTy>&& rhsPtr = makeCombiner(rhs);
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
		boost::mpl::or_<boost::is_same<C1<FuncTy>,Constraint<FuncTy>>, boost::is_same<C1<FuncTy>,CombinerPtr<FuncTy>>>,
		boost::mpl::or_<boost::is_same<C2<FuncTy>,Constraint<FuncTy>>, boost::is_same<C2<FuncTy>,CombinerPtr<FuncTy>>>
	>, CombinerPtr<FuncTy>>::type
operator or(const C1<FuncTy>& lhs, const C2<FuncTy>& rhs) { 
	CombinerPtr<FuncTy> lhsPtr = makeCombiner(lhs);
	CombinerPtr<FuncTy> rhsPtr = makeCombiner(rhs);
	if (!lhsPtr) return rhsPtr;
	if (!rhsPtr) return lhsPtr;
	// FIXME: check whether the iteration vectors of lhs and rhs are compatible for the constraints
	// to be composed
	return makeDisjunction<FuncTy>(lhsPtr, rhsPtr); 
}

namespace {

template <class FuncTy>
struct ConstraintNormalizer : public RecConstraintVisitor<FuncTy,CombinerPtr<FuncTy>> {

	ConstraintNormalizer() { }

	CombinerPtr<FuncTy> visitRawConstraint(const RawConstraint<FuncTy>& rcc) { 
		return makeCombiner( normalize( rcc.getConstraint() ) ); 
	}

	CombinerPtr<FuncTy> visitNegConstraint(const NegConstraint<FuncTy>& ucc) { 
		CombinerPtr<FuncTy> sub = this->visit( ucc.getSubConstraint() );
		assert(sub && "Normalization of sub constraint failed");

		if (std::shared_ptr<NegConstraint<FuncTy>> subCons = 
			std::dynamic_pointer_cast<NegConstraint<FuncTy>>( sub )) 
		{ 
			return subCons->getSubConstraint(); 
		}
		return not_( sub );
	}

	CombinerPtr<FuncTy> visitBinConstraint(const BinConstraint<FuncTy>& bcc) {
		CombinerPtr<FuncTy> lhs = this->visit( bcc.getLHS() );
		assert(lhs && "Failed to normalize lhs of binary constraint");

		CombinerPtr<FuncTy> rhs = this->visit( bcc.getRHS() );
		assert(rhs && "Failed to normalized rhs of binary constraint");

		return bcc.isDisjunction() ? lhs or rhs : lhs and rhs;
	}

};

} // end anonymous namespace 

template <class FuncTy>
inline CombinerPtr<FuncTy> normalize( const CombinerPtr<FuncTy>& cons ) {
	return ConstraintNormalizer<FuncTy>().visit(cons);
}

namespace {

template <class SrcTy, class TrgTy>
struct ConstraintConverter : public RecConstraintVisitor<SrcTy, CombinerPtr<TrgTy>> {

	ConstraintConverter() { }

	CombinerPtr<TrgTy> visitRawConstraint(const RawConstraint<SrcTy>& rcc) { 
		return makeCombiner(Constraint<TrgTy>( 
					static_cast<TrgTy>(rcc.getConstraint().getFunction()), 
					rcc.getConstraint().getType() 
				));  
	}

	CombinerPtr<TrgTy> visitNegConstraint(const NegConstraint<SrcTy>& ucc) { 
		return not_( this->visit(ucc.getSubConstraint()) );
	}

	CombinerPtr<TrgTy> visitBinConstraint(const BinConstraint<SrcTy>& bcc) {
		CombinerPtr<TrgTy> lhs = this->visit(bcc.getLHS());
		assert(lhs && "Error while converting LHS of binary constraint");

		CombinerPtr<TrgTy> rhs = this->visit(bcc.getRHS());
		assert(rhs && "Error while converting RHS of binary constraint");

		return bcc.isDisjunction() ? lhs or rhs : lhs and rhs;
	}
};

} // end anonymous namespace 


template <class SrcTy, class TrgTy>
inline CombinerPtr<TrgTy> 
castTo(const CombinerPtr<SrcTy>& cons, typename boost::enable_if<boost::is_convertible<SrcTy,TrgTy>>::type* dummy = 0) 
{
	return ConstraintConverter<SrcTy, TrgTy>().visit(cons);
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
	typedef CombinerPtr<FuncTy>  				PredicatePtr;
	typedef ConstraintType 	   			  		PredicateType;

	typedef std::pair<PredicatePtr, FuncTy> 	Piece;

	typedef std::vector<Piece> 					Pieces;
	typedef typename Pieces::iterator			iterator;
	typedef typename Pieces::const_iterator		const_iterator;

	Piecewise( const Pieces& pieces = Pieces()) : pieces(pieces) { }

	// Build a piecewise containing only 1 piece
	Piecewise( const PredicatePtr& pred, const FuncTy& trueVal, const FuncTy& falseVal = FuncTy()) 
		: pieces( { Piece(pred, trueVal), 
					Piece(not_(pred), falseVal) 
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






// TO DNF Form

template <class FuncTy>
CombinerPtr<FuncTy> toDNF(const CombinerPtr<FuncTy>& c);

namespace {

template <class FuncTy>
struct DNFNormalizer : public RecConstraintVisitor<FuncTy,CombinerPtr<FuncTy>> {

	DNFNormalizer() { }

	CombinerPtr<FuncTy> visitRawConstraint(const RawConstraint<FuncTy>& rcc) { 
		return makeCombiner( normalize( rcc.getConstraint() ) ); 
	}

	CombinerPtr<FuncTy> visitNegConstraint(const NegConstraint<FuncTy>& ucc) { 
		CombinerPtr<FuncTy>&& sub = this->visit( ucc.getSubConstraint() );
		assert(sub && "Normalization of sub constraint failed");

		if (std::shared_ptr<NegConstraint<FuncTy>> subCons = 
			std::dynamic_pointer_cast<NegConstraint<FuncTy>>( sub )) 
		{ 
			return subCons->getSubConstraint(); 
		}

		if (std::shared_ptr<BinConstraint<FuncTy>> bc = 
			std::dynamic_pointer_cast<BinConstraint<FuncTy>>( sub )) 
		{ 
			CombinerPtr<FuncTy>&& lhs = toDNF( not_(bc->getLHS()) );
			CombinerPtr<FuncTy>&& rhs = toDNF( not_(bc->getRHS()) );
			return (bc->isConjunction() ? toDNF(lhs or rhs) : toDNF(lhs and rhs));
		}
		// already in DNF
		return not_( sub );
	}

	CombinerPtr<FuncTy> visitBinConstraint(const BinConstraint<FuncTy>& bcc) {
		CombinerPtr<FuncTy>&& lhs = this->visit( bcc.getLHS() );
		CombinerPtr<FuncTy>&& rhs = this->visit( bcc.getRHS() );

		// both sides are single elements 
		if (lhs->getCombinerType() < CT_BIN && rhs->getCombinerType() < CT_BIN) {
			return bcc.isDisjunction() ? lhs or rhs : lhs and rhs;
		}

		// The LHS is a single element
		if (lhs->getCombinerType() < CT_BIN) {
			const BinConstraint<FuncTy>& brhs = static_cast<const BinConstraint<FuncTy>&>(*rhs);
			if (bcc.getType() == brhs.getType()) { 
				return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
			}
			if (bcc.isDisjunction()) { 
				return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
			}

			return toDNF( lhs and brhs.getLHS() ) or toDNF( lhs and brhs.getRHS() );
		}

		// The RHS is a single element
		if (rhs->getCombinerType() < CT_BIN) {
			const BinConstraint<FuncTy>& blhs = static_cast<const BinConstraint<FuncTy>&>(*lhs);
			if (bcc.getType() == blhs.getType()) {
				return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
			}
			if (bcc.isDisjunction()) { 
				return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
			}

			return toDNF( rhs and blhs.getLHS() ) or toDNF( rhs and blhs.getRHS() );
		}

		// both sides are binary expressions 
		// if this node is a disjucntion then we already are in normal form and we can quit
		if (bcc.isDisjunction()) { 
			return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
		}

		// then this is a conjunction 
		// if the sub constraints are again conjunctions, then we are in normal form again
		const BinConstraint<FuncTy>& brhs = static_cast<const BinConstraint<FuncTy>&>(*rhs);
		const BinConstraint<FuncTy>& blhs = static_cast<const BinConstraint<FuncTy>&>(*lhs);

		if (brhs.isConjunction() && blhs.isConjunction()) {
			return makeCombiner( BinConstraint<FuncTy>(bcc.getType(), lhs, rhs) ); 
		}
		
		// last case, one of the two is a disjunction
		if (blhs.isDisjunction()) {
			return toDNF(blhs.getLHS() and makeCombiner(brhs)) or 
				   toDNF(blhs.getRHS() and makeCombiner(brhs));
		}

		assert (brhs.isDisjunction());

		return toDNF(brhs.getLHS() and makeCombiner(blhs)) or 
			   toDNF(brhs.getRHS() and makeCombiner(blhs));
	}

};


template <class FuncTy>
struct ConjunctionsCollector : public RecConstraintVisitor<FuncTy,void> {

	std::vector<std::vector<CombinerPtr<FuncTy>>> conjunctions;

	ConjunctionsCollector() : conjunctions(1) { }

	void visitRawConstraint(const RawConstraint<FuncTy>& rcc) { 
		conjunctions.back().push_back( makeCombiner(rcc) );
	}

	void visitNegConstraint(const NegConstraint<FuncTy>& ucc) { 
		assert(ucc.getSubConstraint()->getCombinerType() == CT_RAW && "Constraint not in DNF");
		const RawConstraint<FuncTy>& rc = static_cast<const RawConstraint<FuncTy>&>(*ucc.getSubConstraint());

		conjunctions.back().push_back( 
				makeCombiner(
					Constraint<FuncTy>(
						rc.getConstraint().getFunction(), 
						getLogicNegation(rc.getConstraint().getType()
					)
				)
			)	
		);
	}

	void visitBinConstraint(const BinConstraint<FuncTy>& bcc) {
		this->visit( bcc.getLHS() );
		if (bcc.isDisjunction()) {
			conjunctions.push_back( std::vector<CombinerPtr<FuncTy> >() );
		}
		this->visit( bcc.getRHS() );
	}
};


} // end anonymous namespace 

template <class FuncTy>
inline CombinerPtr<FuncTy> toDNF( const CombinerPtr<FuncTy>& cons ) {
	return DNFNormalizer<FuncTy>().visit(cons);
}

template <class FuncTy>
std::vector<std::vector<CombinerPtr<FuncTy>>> getConjunctions(const CombinerPtr<FuncTy>& c) {
	ConjunctionsCollector<FuncTy> cnv;
	cnv.visit(c);

	return cnv.conjunctions;
}


template <class FuncTy> 
CombinerPtr<FuncTy> fromConjunctions(const std::vector<std::vector<CombinerPtr<FuncTy>>>& conj) {

	CombinerPtr<FuncTy> res;
	for (const auto& c : conj) {
		CombinerPtr<FuncTy> sub;
		for (const auto& curr : c) 
			sub = !sub ? curr : (sub and curr);
		res = !res ? sub : (res or sub);
	}
	return res;

}

template <class FuncTy>
inline CombinerPtr<FuncTy> toConstraint(const std::vector<std::vector<CombinerPtr<FuncTy>>>& disjunctions) {
	CombinerPtr<FuncTy> ret;
	
	for_each(disjunctions, [&] (const std::vector<CombinerPtr<FuncTy>>& cur) {
		CombinerPtr<FuncTy> sub;
		for_each(cur, [&](const CombinerPtr<FuncTy>& cur) { sub = sub ? sub and cur : cur; });
		ret = ret ? ret or sub : sub;
	});

	return ret;
}

} // end utils namespace
} // end insieme namespace
