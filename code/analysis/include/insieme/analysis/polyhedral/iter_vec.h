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
#include <iterator>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/utils/printable.h"

#include <boost/operators.hpp>

namespace insieme { namespace core {

//===== Forward decls ==============================================================================
class Variable;
template<typename T> class Pointer;
typedef Pointer<const Variable> VariablePtr;

class Expression;
template<typename T> class Pointer;
typedef Pointer<const Expression> ExpressionPtr;

} // end core namespace 

namespace analysis { namespace polyhedral {

/**************************************************************************************************
 * Element defines an element appearing in the iteration vector which can be either an iterator or a
 * global parameter.In the IR they are both represented using a Variable.
 *
 * It is important for the polyhedral model to be able to distinguish between an iterator and a
 * parameter, this is required later in the creation of the sets and relationships representing the
 * polyhedron. 
 *************************************************************************************************/
struct Element : 
	public utils::Printable, 
	public boost::equality_comparable<Element> 
{ 
	// The type of a vector element is either an Iterator or a Parameter
	enum Type { ITER, PARAM, CONST };
	
	Element(const Type& type) : type(type) { }

	inline Type getType() const { return type; }

	virtual std::ostream& printTo(std::ostream& out) const = 0;

	bool operator==(const Element& other) const;
	
	bool operator<(const Element& other) const;

	inline bool operator>=(const Element& other) const { return !(*this < other); }

private:
	Type type;
};

/**************************************************************************************************
 * A Variable in a wrapper for an IR Variable. We use the wrapper in order to solve error with the
 * resolution of the == operator by the compiler. 
 *************************************************************************************************/
class Expr : public Element {
	core::ExpressionPtr expr;
public:
	Expr(const Type& type, const core::ExpressionPtr& expr) : Element(type),  expr(expr) { } 

	inline const core::ExpressionPtr& getExpr() const { assert(expr); return expr; } 

	virtual ~Expr() { }
};

/************************************************************************************************** 
 * An Iterator is a variable in the iteration vector which refers to a loop iterator. Iterators are
 * always listed at the beginning of the iterator vector and their order refers to the nesting
 * levels. 
 *************************************************************************************************/
struct Iterator : public Expr {
	Iterator(const core::VariablePtr& var, bool existence=false) : 
		Expr(Element::ITER, var), existence(existence) { } 
	
	inline const core::VariablePtr getVariable() const {
		return getExpr().as<core::VariablePtr>(); 
	}
	
	inline bool operator<(const Iterator& other) const {
		return getVariable()->getId() < other.getVariable()->getId();
	}
	
	// Implements the printable interface
	std::ostream& printTo(std::ostream& out) const;

	inline bool isExistential() const { return existence; }

private:
	bool existence;
};

/**************************************************************************************************
 * A Parameter refers to variable which are global constant in the SCoP. This means that these
 * variables are not loop iterators. In the IR these variables are still represented as Variable, so
 * we use the same base class as Iterators.  
 *************************************************************************************************/
struct Parameter : public Expr {
	Parameter(const core::ExpressionPtr& expr) : Expr(Element::PARAM, expr) { }	
	
	// Implements the Printable interface
	std::ostream& printTo(std::ostream& out) const; 
};

/************************************************************************************************** 
 * The constant part of an iteration domain is by default the last element of the vector and fixed
 * to 1. We define a class to hold this value in order to make easier the representation of an
 * iterator domain. 
 *************************************************************************************************/
struct Constant : public Element {
	Constant() : Element(Element::CONST) { }

	// Implements the Printable interface
	inline std::ostream& printTo(std::ostream& out) const { return out << "1"; }
	virtual ~Constant() { }
};

/**************************************************************************************************
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
 *************************************************************************************************/
struct IterationVector : public utils::Printable, 
	public boost::equality_comparable<IterationVector> {

	typedef std::vector<Iterator> IterVec;
	typedef std::vector<Parameter> ParamVec;

private:
	IterVec iters;					// ordered list of iterators

	ParamVec params;				// ordered list of parameters
	Constant constant;				// constant part set to 1 (implicit) 

	template <class T>							
	inline int getIdxFrom(const T& elem, const std::vector<T>& vec) const {
		auto fit = std::find(vec.begin(), vec.end(), elem);
		if (fit != vec.end()) { return fit - vec.begin(); }
		return -1;
	}

	template <class T>							
	inline size_t addTo(const T& elem, std::vector<T>& vec) {
		int idx = getIdxFrom(elem, vec);
		if (idx != -1) { return idx; }

		// Append the new element to the vector 
		vec.push_back(elem);
		return vec.size() - 1;
	}

public:

	/**********************************************************************************************
	 * Class utilized to build iterators over an iteration vector. Because the internal
	 * representation of the iteration vector is non linear in memory the iterator can be used to
	 * access all the element of the iterator in using the iterator interface. 
	 *********************************************************************************************/
	class iterator : public boost::random_access_iterator_helper<iterator, Element> {

		const IterationVector& iterVec;
		IterVec::const_iterator iterIt;
		ParamVec::const_iterator paramIt;
		bool constant, valid;
	
		void inc(size_t n);
	public:
		iterator(const IterationVector& 	iterVec, 
				 IterVec::const_iterator 	iterIt, 
				 ParamVec::const_iterator 	paramIt, 
				 bool 						valid=true) 
		: iterVec(iterVec), iterIt(iterIt), paramIt(paramIt), constant(valid), valid(valid) { }

        const Element& operator*() const;

        inline iterator& operator++() { inc(1); return *this; }
		inline iterator& operator+=(size_t val) { inc(val); return *this; }

        inline bool operator==(const iterator& rhs) const { 
			return &iterVec == &rhs.iterVec && iterIt == rhs.iterIt && 
				paramIt == rhs.paramIt && constant == rhs.constant && valid == rhs.valid;
		}
	};

	// Allows the iterator class to access the private part of the IterationVector class 
	friend class iterator;

	typedef IterVec::const_iterator iter_iterator;
	typedef ParamVec::const_iterator param_iterator;

	IterationVector( const std::vector<core::VariablePtr>& iter = std::vector<core::VariablePtr>(), 
					 const std::vector<core::ExpressionPtr>& param = std::vector<core::ExpressionPtr>() ) 
	{
		for_each(iter, [&](const core::VariablePtr& cur) { add( Iterator(cur) ); });

		for_each(param, [&](const core::ExpressionPtr& cur) { add( Parameter(cur) ); });
	}

	/**
	 * Appends an iterator to the list of iterators for this iteration vector, and returns its
	 * position in the iteration vector. 
	 *
	 * In the case the iterator is already present, the index of the element is returned. 
	 */
	inline size_t add(const Iterator& iter) { 
		assert((getIdxFrom(iter, iters) != -1 || getIdx(iter) == -1) && "Variable already among the iterators");
		return addTo(iter, iters); 
	}
	inline size_t add(const Parameter& param) { 
		assert((getIdxFrom(param, params) != -1 || getIdx(param) == -1) && "Variable exists among the iterators");
		return addTo(param, params) + iters.size(); 
	}

	inline size_t add(const Element& elem) { 
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
	inline int getIdx(const core::ExpressionPtr& var) const {
		if (var->getNodeType() == core::NT_Variable) {
			int idx = getIdx( Iterator(core::static_pointer_cast<const core::Variable>(var)) );
			if ( idx != -1 ) {
				assert(getIdx( Parameter(var) ) == -1 && 
						"Variable is both among the iterators and parameters.");
				return idx;
			}
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
	inline iterator begin() const { 
		return iterator(*this, iters.begin(), params.begin()); 
	}
	inline iterator end() const { 
		return iterator(*this, iters.end(), params.end(), false); 
	}

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

	bool operator<(const IterationVector& other) const;

	// Implements the Printable interface
	std::ostream& printTo(std::ostream& out) const;

	// Tests whether the given expression is part of this iteration vector
	bool contains(const core::ExpressionPtr& expr) const;
};

// Merges two iteration vectors (a and b) to create a new iteration vector which contains both the
// elements of a and b. 
IterationVector merge(const IterationVector& a, const IterationVector& b);

typedef std::vector<size_t> IndexTransMap;

/**************************************************************************************************
 * Creates a transformation map which maps the index in the iteration vector src into positions into
 * iteration vector trg. This map can be used to efficiently convert AffineFunction expressed in the
 * base of src vector in the new base (trg vector).
 *
 * Src must be <= than trg and all the element present in src must appear in trg. In contrary case
 * an exception will be thrown. The size of the returned transformation map is always equal to the
 * size of the src iteration vector.
 *************************************************************************************************/
const IndexTransMap transform(const IterationVector& trg, const IterationVector& src);

/**
 * Creates a new iteration vector which contains the iterators and parameters of the given intVec
 * but where the existentially qualified variables have been removed. 
 */
IterationVector removeExistQualified(const IterationVector& iterVec);

} } } // end insieme::analysis::polyhedral namespace

namespace std {

namespace poly = insieme::analysis::polyhedral;

template <>
struct hash<poly::Iterator> {
	size_t operator()(const poly::Iterator& it) const { 
		return (*it.getExpr()).hash();
	}
};

template <>
struct hash<poly::Parameter> {
	size_t operator()(const poly::Parameter& it) const { 
		return (*it.getExpr()).hash();
	}
};

} // end std namespace 
