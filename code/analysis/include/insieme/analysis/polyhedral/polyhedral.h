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

#include <array>
#include <iterator>
#include <stdexcept>
#include <memory>
#include <set>

#include "insieme/core/ast_node.h"
#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"

namespace insieme {
namespace core {

class Variable;
template<typename T> class Pointer;
typedef Pointer<const Variable> VariablePtr;

class Expression;
template<typename T> class Pointer;
typedef Pointer<const Expression> ExpressionPtr;

}
namespace analysis {
namespace poly {

//===== Exceptions ==============================================================
struct NotAffineExpr : public std::logic_error {
	const core::ExpressionPtr expr;
	NotAffineExpr(const core::ExpressionPtr& expr);
	~NotAffineExpr() throw () { }
};

struct VariableNotFound : public std::logic_error {
	const core::VariablePtr var;
	VariableNotFound(const core::VariablePtr& var);
	~VariableNotFound() throw () { }
};

struct IteratorNotValid : public std::logic_error { 
	IteratorNotValid() : std::logic_error("Iterator not valid!") { }
	~IteratorNotValid() throw () { }
};

/**
 * Element defines an element appearing in the iteration vector which can be either an iterator or a
 * global parameter.In the IR they are both represented using a Variable.
 *
 * It is important for the polyhedral model to be able to distinguish between an iterator and a
 * parameter, this is required later in the creation of the sets and relationships representing the
 * polyhedron. 
 */
struct Element : public utils::Printable, public boost::equality_comparable<Element> { 
	// The type of a vector element is either an Iterator or a Parameter
	enum Type { ITER, PARAM, CONST };
	
	Element(const Type& type) : type(type) { }

	inline Type getType() const { return type; }
	virtual std::ostream& printTo(std::ostream& out) const = 0;

	bool operator==(const Element& other) const;
    bool operator<(const Element& other) const;
private:
	Type type;
};

/** 
 * A Variable in a wrapper for an IR Variable. We use the wrapper in order to solve error with the
 * resolution of the == operator by the compiler. 
 */
class Variable : public Element {
	core::VariablePtr var;
public:
	Variable(const Type& type, const core::VariablePtr& var) : Element(type),  var(var) { } 
	core::VariablePtr getVariable() const { return var; } 
	virtual ~Variable() { }
};

/** 
 * An Iterator is a variable in the iteration vector which refers to a loop iterator. Iterators are
 * always listed at the beginning of the iterator vector and their order refers to the nesting
 * levels. 
 */
struct Iterator : public Variable {
	Iterator(const core::VariablePtr& var) : Variable(Element::ITER, var) { } 
	
	// Implements the printable interface
	std::ostream& printTo(std::ostream& out) const;
};

/**
 * A Parameter refers to variable which are global constant in the SCoP. This means that these
 * variables are not loop iterators. In the IR these variables are still represented as Variable, so
 * we use the same base class as Iterators.  
 */
struct Parameter : public Variable {
	Parameter(const core::VariablePtr& var) : Variable(Element::PARAM, var) { }	
	
	// Implements the Printable interface
	std::ostream& printTo(std::ostream& out) const; 
};

/** 
 * The constant part of an iteration domain is by default the last element of the vector and fixed
 * to 1. We define a class to hold this value in order to make easier the representation of an
 * iterator domain. 
 * */
struct Constant : public Element {
	Constant() : Element(Element::CONST) { }

	// Implements the Printable interface
	std::ostream& printTo(std::ostream& out) const { return out << "1"; }
	virtual ~Constant() { }
};

/**
 * An iteration vector is an order set of elements (either iterators or parameters) which defines
 * the position of a specific variable in the domain matrix and transformation matrix which is built
 * on top of this vector. 
 *
 * The order of the variables in the vector is usually required (by the polyhedral libraries) to be
 * ordered in the following way: 
 *
 * (iter0,...iterN | param0, ..., paramM | 1)
 *
 * Which defines an iteration vector of size N+M+1, the first N elements are iterators, followed by
 * M parameters and the last element is the constant part always set to 1. 
 *
 * Each Polyhedral data structure needs to refer to this array to know the element associated to a
 * particular position. 
 *
 * Because when the iteration vector is built we don't know the exact size of N and M (because we
 * may encounter new iterators or parameters as we build the iteration domain) we chose a
 * representation which allows the size of the vector to grow without invalidating already generated
 * polyhedron.
 */
class IterationVector : public utils::Printable, 
	public boost::equality_comparable<IterationVector> {

	typedef std::vector<Iterator> IterVec;
	IterVec iters;					// ordered list of iterators

	typedef std::vector<Parameter> ParamVec;
	ParamVec params;				// ordered list of parameters
	Constant constant;				// constant part set to 1 (implicit) 

	template <class T>							
	int getIdxFrom(const T& elem, const std::vector<T>& vec) const {
		auto fit = std::find(vec.begin(), vec.end(), elem);
		if (fit != vec.end())
			return fit - vec.begin();
		return -1;
	}

	template <class T>							
	size_t addTo(const T& elem, std::vector<T>& vec) {
		int idx = getIdxFrom(elem, vec);
		if (idx != -1)
			return idx;

		// Append the new element to the vector 
		vec.push_back(elem);
		return vec.size() - 1;
	}

public:

	/**
	 * Class utilized to build iterators over an iteration vector. Because the internal
	 * representation of the iteration vector is non linear in memory the iterator can be used to
	 * access all the element of the iterator in using the iterator interface. 
	 * */
	class iterator : public boost::random_access_iterator_helper<iterator, Element> {

		const IterationVector& iterVec;
		IterVec::const_iterator iterIt;
		ParamVec::const_iterator paramIt;
		bool constant, valid;
	
		void inc(size_t n);
	public:
		iterator(const IterationVector& iterVec, IterVec::const_iterator iterIt, 
				ParamVec::const_iterator paramIt, bool valid=true) :
			iterVec(iterVec), iterIt(iterIt), paramIt(paramIt), constant(valid), 
			valid(valid) { }

        const Element& operator*() const;

        iterator& operator++() { inc(1); return *this; }
		iterator& operator+=(size_t val) { inc(val); return *this; }

        bool operator==(const iterator& rhs) const { 
			return &iterVec == &rhs.iterVec && iterIt == rhs.iterIt && 
				paramIt == rhs.paramIt && constant == rhs.constant && valid == rhs.valid;
		}
	};

	// Allows the iterator class to access the private part of the IterationVector class 
	friend class iterator;

	typedef IterVec::const_iterator iter_iterator;
	typedef ParamVec::const_iterator param_iterator;

	IterationVector() { }

	/**
	 * Appends an iterator to the list of iterators for this iteration vector, and returns its
	 * position in the iteration vector. 
	 *
	 * In the case the iterator is already present, the index of the element is returned. 
	 */
	inline size_t add(const Iterator& iter) { return addTo(iter, iters); }
	inline size_t add(const Parameter& param) { return addTo(param, params) + iters.size(); }

	size_t add(const Element& elem) { 
		return elem.getType() == Element::ITER ? 
				add(static_cast<const Iterator&>(elem)) : add(static_cast<const Parameter&>(elem)); 
	}

	/**
	 * Returns the index of an element inside the iteration vector. 
	 * 
	 * If the element is not in the iteration vector, -1 is returned
	 */
	int getIdx(const Element& elem) const; 

	/** 
	 * Search for a Variable inside this iteration vector, check if the variable is within the
	 * iterators and the parameters, it returns -1 id the variable was not found
	 * */
	int getIdx(const core::VariablePtr& var) const {
		int idx = getIdx( Iterator(var) );
		if ( idx != -1 ) {
			assert(getIdx( Parameter(var) ) == -1 && "Variable is both among the iterators and parameters.");
			return idx;
		}
		return getIdx( Parameter(var) );
	}

	// Retrieve the overall size for this iteration vector
	inline size_t size() const { return iters.size() + params.size() + 1; }
	// Retrieve the number of iterators and parameters for this iteration vector
	inline size_t getIteratorNum() const { return iters.size(); }
	inline size_t getParameterNum() const { return params.size(); }

	// Returns an iterator over the Elements of this iteration vector,
	// the elements are returned according to the order defined as follows:
	// (iter0, iter1 ... iterN | param0, param1, ... paramM | 1)
	inline iterator begin() const { return iterator(*this, iters.begin(), params.begin()); }
	inline iterator end() const { return iterator(*this, iters.end(), params.end(), false); }

	// Returns an iterator over the iterators of this iteration vector:
	// (iter0, iter1, ... iterN)
	inline iter_iterator iter_begin() const { return iters.begin(); }
	inline iter_iterator iter_end() const { return iters.end(); }

	// Returns an iterator over the parameters of this iteration vector:
	// (param0, param1, ... paramM)
	inline param_iterator param_begin() const { return params.begin(); }
	inline param_iterator param_end() const { return params.end(); }

	const Element& operator[](size_t idx) const;

	bool operator==(const IterationVector& other) const;

	// Implements the Printable interface
	std::ostream& printTo(std::ostream& out) const;
};

// Merges two iteration vectors (a and b) to create a new iteration vector which contains both the
// elements of a and b. 
IterationVector merge(const IterationVector& a, const IterationVector& b); 

typedef std::vector<size_t> IndexTransMap;

/**
 * Creates a transformation map which maps the index in the iteration vector src into positions into
 * iteration vector trg. This map can be used to efficiently convert AffineFunction expressed in the
 * base of src vector in the new base (trg vector).
 *
 * Src must be <= than trg and all the element present in src must appear in trg. In contrary case
 * an exception will be thrown. The size of the returned transformation map is always equal to the
 * size of the src iteration vector.
 */
const IndexTransMap transform(const IterationVector& trg, const IterationVector& src);

/**
 * AffineFunction represents an affine function based on an iteration vector. An
 * affine linear function is a function in the form:
 *
 * 3 * a + 2 * b ... + 0 
 *
 * Variables are either iterators or parameters. The representation is done keeping a vector of
 * coefficients referring to an iteration vector. For example considering the iteration vector of
 * the form (a, b, ... 1), the coefficient matrix we need to store to represent the previous affine
 * function is: (3, 2, ... 0). 
 *
 * Because we want to be able to change the dimensions of the iteration vector during the
 * construction of a SCoP, the affine function should refer to an iteration vector which size may
 * change. But because new iterators or parameters are always append, we can easily create the new
 * coefficient matrix for the mutated iteration vector, thanks to the sep member.  
 */
class AffineFunction : public utils::Printable, 
	public boost::equality_comparable<AffineFunction> { 
	// Iteration Vector to which this function refers to 
	const IterationVector& iterVec;

	// List of integer coefficients (the polyhedral model does not allow to represent non integer
	// coefficients)
	std::vector<int> coeffs;

	// Keeps the information of the number of iterators the iteration vector had when this affine
	// function was created. This will allow us to produce the updated coefficient matrix in the
	// case new parameters or iterators are added to the iterVec. 
 	size_t sep;

	/**
	 * Converts an index in the iteration vector pointing to element E to an index on the
	 * coefficient vector which points to the coefficient value associated to element E.
	 *
	 * Returns -1 in the case E was created after this instance of affine function is generated. In
	 * that case the coefficient value for the index is by default zero.  
	 */
	int idxConv(size_t idx) const;
	
public:
	template <class T>
	friend class ConstraintSet; 

	typedef std::pair<const Element&, int> Term;
	/**
	 * Class utilized to build iterators over Affine Functions. 
	 *
	 * The iterator returns a pair<Element,int> containing the element and the
	 * coefficient associated to it. 
	 *
	 */
	struct iterator : public boost::forward_iterator_helper<iterator, Term> {

		const IterationVector& iterVec;

		const AffineFunction& af;
		size_t iterPos;

		iterator(const IterationVector& iterVec, const AffineFunction& af, size_t iterPos=0) : 
			iterVec(iterVec), af(af), iterPos(iterPos) { }

		Term operator*() const; 
		iterator& operator++();

		bool operator==(const iterator& rhs) const { 
			return &iterVec == &rhs.iterVec && &af == &rhs.af && iterPos == rhs.iterPos;
		}
	};

	AffineFunction(const IterationVector& iterVec) : 
		iterVec(iterVec), coeffs( iterVec.size() ), sep( iterVec.getIteratorNum() ) { }

	AffineFunction(IterationVector& iterVec, const insieme::core::ExpressionPtr& expr);

	// Creates a copy of this affine function based on a different iteration
	// vector. This is necessary when composing constrains for building
	// iteration domains 
	// AffineFunction(const IterationVector& newIterVec, const AffineFunction& other);

	AffineFunction(const AffineFunction& other) : iterVec(other.iterVec), coeffs(other.coeffs), sep(other.sep) { }

	inline const IterationVector& getIterationVector() const { return iterVec; }

	// Set a coefficient for an iterator. 
	void setCoefficient(const Iterator& iter, int coeff) {
		size_t idx = iterVec.getIdx(iter);
		assert(idx<sep && 
			"Iterator vector has been updated before the affine function has been created."); 
			// FIXME define exception class
		coeffs[idx] = coeff;
	}

	// Sets the coefficient value for a parameter
	void setCoefficient(const Parameter& param, int coeff) {
		size_t idx = iterVec.getIdx(param);
		assert(idx<coeffs.size()-1 && 
			"Iterator vector has been updated before the affine function has been created."); 
			// FIXME define exception class
		coeffs[idx] = coeff;
	}

	// Set the constant part coefficient 
	void setConstantPart(int coeff) { coeffs.back() = coeff; }

	iterator begin() const { return iterator(iterVec, *this); }
	iterator end() const { return iterator(iterVec, *this, iterVec.size()); }

	int getCoeff(const core::VariablePtr& var) const;
	inline int getConstCoeff() const { return coeffs.back(); }

	inline size_t size() const { return iterVec.size(); } 

	// Implements the Printable interface 
	std::ostream& printTo(std::ostream& out) const;

	bool operator==(const AffineFunction& other) const;

	/**
	 * Creates a copy of this affine function using another iteration vector as a base. This method
	 * can be invoked both providing a transformation map. In the case the transfomration map is not
	 * provided, it will be recomputed by the method. 
	 *
	 * The created affine function will be based on the iteration vector (iterVec), meaning that the
	 * user is responsable for the instance of iterVec to remain alive as long as the created Affine
	 * function is utilized. 
	 */
	AffineFunction 
	toBase(const IterationVector& iterVec, const IndexTransMap& idxMap = IndexTransMap()) const; 
};

/**
 * A constraint is a linear affine expression limiting the polyhedron. A set of constraints will
 * define an iteration domain which is our polyhedron. A constraint is usually represented as an
 * inequality, i.e. f(x) <= 0, however we allow here for a more general representation allowing any
 * sort of constraint (==, !=, <, >, <= and >=) to be represented. 
 */
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

//===== ConstraintCombiner ========================================================================

// The constraint combiner has the task to combine multiple constraints into conjunctions (AND) or
// disjunctions (OR) of constraints which can be either in positive form of negated. Because
// arbitrary nested structures are possible, we built a binary tree containing constraints. 
//
// Forward declaration for the Constraint combiner 
class ConstraintCombiner;
typedef std::shared_ptr<ConstraintCombiner> ConstraintCombinerPtr; 

// forward declaration for the Constraint visitor 
struct ConstraintVisitor; 

/**
 * This class has the purpose to create conjunctions and/or disjunctions of constraints. This allows
 * to represent the domain spawned by control operations with a composed conditional expression
 */
struct ConstraintCombiner : public utils::Printable {
	// implements a simple double dispatch visitor for the Composite  
	virtual void accept(ConstraintVisitor& v) const = 0; 

	std::ostream& printTo(std::ostream& out) const;
};

/**
 * This class is a wrapper for a plain Constraint. Utilized to combine constraints in a composite
 * like structure.
 */
class RawConstraintCombiner : public ConstraintCombiner {
	Constraint constraint; 
public:
	RawConstraintCombiner(const Constraint& constraint) : 
		ConstraintCombiner(), constraint(constraint) { }
	
	// Returns the constraint embodied in this wrapper class
	inline const Constraint& getConstraint() const { return constraint; }
	
	void accept(ConstraintVisitor& v) const;
};

/**
 * This class represents the negation of a constraint. 
 */
class NegatedConstraintCombiner : public ConstraintCombiner {
	ConstraintCombinerPtr subComb;
public:
	NegatedConstraintCombiner(const ConstraintCombinerPtr& comb) :
		ConstraintCombiner( ), subComb( comb ) { }

	// Returns the negated constraint 
	inline const ConstraintCombinerPtr& getSubConstraint() const { return subComb; }

	void accept(ConstraintVisitor& v) const;
};

/**
 * This class represent the combination of two constraints which can be either a combined through a
 * AND or a OR operator. 
 */
class BinaryConstraintCombiner : public ConstraintCombiner {
public:
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

/** 
 * Visitor class used to visit a combination of constraints. Because constraints are combined
 * together in a composite (tree like) structure, it is therefore easier to visit the structure by
 * means of a visitor. 
 *
 * The implementation of the visitor is based on double-dispatch. 
 */
struct ConstraintVisitor {
	
	// Visits a raw node (which contains a raw constraint)
	virtual void visit(const RawConstraintCombiner& rcc) { }
	
	// Visits a negated constraints 
	virtual void visit(const NegatedConstraintCombiner& ucc) {
		ucc.getSubConstraint()->accept(*this);
	}

	// Visits a combination of constraints which can either be a conjunction or
	// a disjunction 
	virtual void visit(const BinaryConstraintCombiner& bcc) {
		bcc.getLHS()->accept(*this); bcc.getRHS()->accept(*this);
	}
};

namespace {

template <class... All>
class Combiner;

// Combiner class takes a list of constraints and assembles them together either in a conjunction or
// a disjunction (depending on the provided type) and returns a pointer to the combiner containing
// the constraints 
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

// Utility function to create negation of constraints. Both of plain constraints (by wrapping them
// on a raw constraint first) and negation of constraints 
ConstraintCombinerPtr negate(const ConstraintCombinerPtr& cc);
ConstraintCombinerPtr negate(const Constraint& c);

ConstraintCombinerPtr makeCombiner(const Constraint& c);

// Makes a copy of the constraint cc changing the base vector to the iteration vector trgVec. 
ConstraintCombinerPtr cloneConstraint(const IterationVector& trgVec, const ConstraintCombinerPtr& cc);

// Defines a list of constraints stored in a vector
typedef std::vector<Constraint> ConstraintList;

// The iteration domain is the class which defines the shape of the polyhedron, the set of integer
// points of an N-dimensional plane are delimited by affine linear functions which define a convex
// region, therefore the polyhedron. 
struct IterationDomain : public utils::Printable {
	IterationDomain(const IterationVector& iterVec, const ConstraintCombinerPtr& combiner) : 
		iterVec(iterVec), constraints(combiner) { }

	std::ostream& printTo(std::ostream& out) const {
		return out << "Iteration Domain: \n\tIV: " << iterVec << "\n\tCONS: [ " << *constraints << " ]";
	}

private:
	IterationVector 		iterVec;
	ConstraintCombinerPtr	constraints; 
};

// Scheduling functions defines the order of statements in the program
// struct ScatteringFunction  { 
//	
//	ScatteringFunction(const IterationVector& iterVec, const std::vector<EqualityConstraint>& clist) :
//		ConstraintSet<EqualityConstraint>(iterVec, clist) { }
//	
//	std::ostream& printTo(std::ostream& out) {
//		out << "ScatteringFunction: ";
//	   	ConstraintSet<EqualityConstraint>::printTo(out);
//		return out;
//	}
//
//};

} // end poly namespace
} // end analysis namespace
} // end insieme namespace 


namespace std {
std::ostream& operator<<(std::ostream& out, const insieme::analysis::poly::AffineFunction::Term& c);
} // end std namespace 


