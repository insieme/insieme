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
#include <unordered_set>
#include <cmath>
#include <algorithm>

#include <utility>
#include <type_traits>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace analysis {
namespace dfa {


namespace {

template <class T>
struct set_base_traits { typedef T type; };

} // end anonymous namespace 


/**
 * Because Dataflow Analysis works on symbolic domains we define a set of classes used to store
 * symbolically some of the mathematical domains often used in dataflow analysis: powerset,
 * cartesian products
 *
 * The main goal of these representation is to be able to test whether a value generated by a
 * transfer function of the dataflow analysis (defined by the user) still belongs to the domain 
 * on which the dataflow analysis was defined. 
 */
template <class T>
struct SymbolicSet { 

	/** 
	 * Checks whether an element is contained in this Symbolic Set
	 */
	virtual bool contains(const T& elem) const = 0;

	/**
	 * Checks whether this set is bounded or not
	 */
	virtual bool bounded() const = 0;

};


/**
 * Predicates which test whether an element is contained in a specific set. 
 * This predicate is implemented as a type-traits so that new type of sets can
 * be easily supported. 
 *
 * For legacy STL sets the predicate uses the find method while for symboli sets
 * the constains() method is used.
 */
template <class T, class Cont>
typename std::enable_if<std::is_base_of<SymbolicSet<T>, Cont>::value, bool>::type
contains(const Cont& cont, const T& elem) {
	return cont.contains(elem);
}

template <class T, class Cont>
typename std::enable_if<!std::is_base_of<SymbolicSet<T>, Cont>::value, bool>::type
contains(const Cont& cont, const T& elem) {
	return cont.find(elem) != cont.end();
}



template <class Cont>
typename std::enable_if<std::is_base_of<SymbolicSet<typename Cont::value_type>, Cont>::value, bool>::type
isBounded(const Cont& cont) {
	return cont.bounded();
}

template <class Cont>
typename std::enable_if<!std::is_base_of<SymbolicSet<typename Cont::value_type>, Cont>::value, bool>::type
isBounded(const Cont& cont) { return true; }




template <class Cont>
typename std::enable_if<std::is_base_of<SymbolicSet<typename Cont::value_type>, Cont>::value, typename Cont::base_type>::type
expand(const Cont& cont) {
	return cont.expand();
}

template <class Cont>
typename std::enable_if<!std::is_base_of<SymbolicSet<typename Cont::value_type>, Cont>::value, Cont>::type
expand(const Cont& cont) { return cont; }


/**
 * Implementation of a generic set based on existing implementations.
 */
template <class T, template <typename... R> class Impl=std::unordered_set>
struct Set : public SymbolicSet<T>, public Impl<T> {

	typedef T 		value_type;
	typedef Impl<T>	base_type;

	/** 
	 * Creates an empty set
	 */
	Set() { }

	Set(const std::initializer_list<T>& elem) : Impl<T>(elem) { }
	
	template <class IterT>
	Set(const IterT& begin, const IterT& end) : Impl<T>(begin, end) { }

	bool contains(const T& elem) const { 
		return std::find(Impl<T>::begin(), Impl<T>::end(), elem) != Impl<T>::end(); 
	}

	bool bounded() const { return true; }
};


/** 
 * A domain set is a set of elements which belongs to domain Dom
 *
 * For example, if Dom=int then the DomainSet is the set of possible integers values. 
 */
template <class Dom>
struct DomainSet : public SymbolicSet<Dom> {

	typedef Dom value_type;

	bool contains(const Dom& d) const { return true; }

	bool bounded() const { return false; }
};


/**
 * Implementation of a symbolic power-set.
 *
 * In mathematics, the power set (or powerset) of any set S, written , P(S) or 2^S, is the set of
 * all subsets of S, including the empty set and S itself. In axiomatic set theory, the existence of
 * the power set of any set is postulated by the axiom of power set.
 */
template <class BaseSet>
class PowerSet: public SymbolicSet<typename set_base_traits<BaseSet>::type> {

	BaseSet base;
public:

	typedef typename set_base_traits<BaseSet>::type value_type;
	
	/** 
	 * Creates an empty power-set
	 */
	PowerSet() { }

	PowerSet(const BaseSet& base_set) : base(base_set) { }

	const BaseSet& getBaseSet() const { return base; }

	size_t size() const { return pow(2,base.size()); }
	
	bool contains(const value_type& elem) const { 
		return all(elem.begin(), elem.end(), 
				[&](const typename value_type::value_type& elem) { return dfa::contains(base,elem); });
	}

	bool bounded() const { return dfa::isBounded(base); }
};



namespace {

template <class BaseSet>
struct set_base_traits<PowerSet<BaseSet>> { typedef typename PowerSet<BaseSet>::base_type type; };

} // end anonymous namespace 

template <class BaseSet> 
PowerSet<BaseSet> makePowerSet(const BaseSet& set) { return PowerSet<BaseSet>(set); }

/**
 * Cartesian-Product of 2 sets 
 */
template <class BaseSet1, class BaseSet2>
class CartProdSet : 
	public SymbolicSet< std::tuple<typename BaseSet1::value_type, typename BaseSet2::value_type> > 
{

	BaseSet1 base1;
	BaseSet2 base2;

public:

	typedef std::tuple<typename BaseSet1::value_type, typename BaseSet2::value_type> value_type;
	// TODO: provide the ability to let the user specify the type of set
	typedef std::set<value_type> base_type;

	/** 
	 * Creates an empty cartesian-product
	 */
	CartProdSet() { }

	CartProdSet(const BaseSet1& base1, const BaseSet2& base2) : 
		base1(base1), base2(base2) { }

	bool contains(const value_type& elem) const {
		return dfa::contains(base1, std::get<0>(elem)) && 
			   dfa::contains(base2, std::get<1>(elem));
	}

	const BaseSet1& getLeftBaseSet() const { return base1; }

	const BaseSet2& getRightBaseSet() const { return base2; }

	size_t size() const { return base1.size() * base2.size(); }

	base_type expand() const { 

		auto bt1 = dfa::expand(base1);
		auto bt2 = dfa::expand(base2);

		base_type res;
		for(auto& it1 : bt1) 
			for (auto& it2 : bt2) {
				res.insert( std::make_tuple(it1,it2) ); 	
			}

		return res;
	}

	bool bounded() const { return dfa::isBounded(base1) && dfa::isBounded(base2); }
};

namespace {

template <class Base1, class Base2>
struct set_base_traits<CartProdSet<Base1,Base2>> {
	typedef typename CartProdSet<Base1,Base2>::base_type type;
};

} // end anonymous namespace 

template <class Base1, class Base2> 
CartProdSet<Base1, Base2> makeCartProdSet(const Base1& set1, const Base2& set2) { 
	return CartProdSet<Base1, Base2>(set1, set2);
}

namespace {

// Function utilized to split a tuple into 2 tuples 

template <size_t B, class SRC, class DEST> 
class CopyHead ;

template <size_t E, class... T1, class... T2> 
struct CopyHead<E,std::tuple<T1...>,std::tuple<T2...>> {
	
	static void copy(const std::tuple<T1...,T2...>& src, std::tuple<T1...>& res) {
		std::get<E>(res) = std::get<E>(src);
		CopyHead<E-1,std::tuple<T1...>, std::tuple<T2...>>::copy(src, res);
	}

};

template <class... T1, class... T2> 
struct CopyHead<0,std::tuple<T1...>,std::tuple<T2...>> {
	
	static void copy(const std::tuple<T1...,T2...>& src, std::tuple<T1...>& res) {
		std::get<0>(res) = std::get<0>(src);
	}
	
};

template <size_t B, class SRC, class DEST> 
class CopyTail ;

template <size_t E, class... T1, class... T2> 
struct CopyTail<E,std::tuple<T1...>,std::tuple<T2...>> {
	
	static void copy(const std::tuple<T1...,T2...>& src, std::tuple<T2...>& res) {
		std::get<E>(res) = std::get<E+sizeof...(T1)>(src);
		CopyTail<E-1,std::tuple<T1...>, std::tuple<T2...>>::copy(src, res);
	}


};

template <class... T1, class... T2> 
struct CopyTail<0,std::tuple<T1...>,std::tuple<T2...>> {
	
	static void copy(const std::tuple<T1...,T2...>& src, std::tuple<T2...>& res) {
		std::get<0>(res) = std::get<sizeof...(T1)>(src);
	}

};

template <class T1, class T2, class T3>
std::pair<T1,T2> split(const T3& t) {

	T1 t1;
	T2 t2;

	CopyHead<std::tuple_size<T1>::value-1, T1, T2>::copy(t, t1);
	CopyTail<std::tuple_size<T2>::value-1, T1, T2>::copy(t, t2);

	return std::make_pair(t1, t2);
}


template <class... T>
struct TupleMerger;


template <class ...T1, class... T2>
struct TupleMerger<std::tuple<T1...>, std::tuple<T2...>> {

	typedef std::tuple<T1..., T2...> type;

};

} // end anonymous namespace 


/**
 * Specialization for Cartsian Product among 2 Cartesian Products. 
 *
 * Because A x B produces a set of tuples in the form (a,b) where a in A and b in B. In the case of
 * a cartesian product between (A x B) and himself what we would like to have, instead of pair of
 * pairs ((a,b),(a,b)), a quadruple (a,b,a,b) where a is in A and B is in B. 
 * This is obtained by 3 specialization of the CartProdSet class which are following. 
 *
 * The three classes covers the 3 cases, i.e.:
 * -------------------------------------------
 * 1) (AxB)x(CxD) -> (a,b,c,d)
 * 2) (AxB)xC	  -> (a,b,c)
 * 3) Ax(BXC)	  -> (a,b,c)
 * -------------------------------------------
 */
template <class Base1, class Base2, class Base3, class Base4>
class CartProdSet<CartProdSet<Base1,Base2>, CartProdSet<Base3,Base4>> :
	public SymbolicSet< 
			typename TupleMerger<
				typename CartProdSet<Base1,Base2>::value_type, 
				typename CartProdSet<Base3,Base4>::value_type
			>::type
		   > 
{

	CartProdSet<Base1,Base2> base1;
	CartProdSet<Base3,Base4> base2;

public:

	typedef CartProdSet<Base1,Base2> BaseSet1;
	typedef CartProdSet<Base3,Base4> BaseSet2;

	typedef typename CartProdSet<Base1,Base2>::value_type Tuple1;
	typedef typename CartProdSet<Base3,Base4>::value_type Tuple2;

	typedef typename TupleMerger<Tuple1,Tuple2>::type value_type;

	// Creates an empty cartesian-product
	CartProdSet() { }

	CartProdSet(const BaseSet1& b1, const BaseSet2& b2) : base1(b1), base2(b2) { }

	bool contains(const value_type& elem) const {
		Tuple1 t1;Tuple2 t2;

		std::tie(t1, t2) = split<Tuple1,Tuple2>(elem);
		return dfa::contains(base1, t1) && dfa::contains(base2, t2);
	}

	const BaseSet1& getLeftBaseSet() const { return base1; }

	const BaseSet2& getRightBaseSet() const { return base2; }

	size_t size() const { return base1.size() * base2.size(); }

	bool bounded() const { return dfa::isBounded(base1) && dfa::isBounded(base2); }
};

/** 
 * Specialization for the case (AxB)xC
 */
template <class Base1, class Base2, class Base3>
class CartProdSet<CartProdSet<Base1,Base2>, Base3> :
	public SymbolicSet< 
			typename TupleMerger<
				typename CartProdSet<Base1,Base2>::value_type, 
				std::tuple<typename Base3::value_type>
			>::type > 
{

	CartProdSet<Base1,Base2> base1;
	Base3 base2;

public:

	typedef CartProdSet<Base1,Base2> BaseSet1;
	typedef Base3					 BaseSet2;

	typedef typename CartProdSet<Base1,Base2>::value_type Tuple1;
	typedef typename std::tuple<typename BaseSet2::value_type> Tuple2;

	typedef typename TupleMerger<Tuple1,Tuple2>::type value_type;

	typedef std::set<value_type> base_type;

	// Creates an empty cartesian-product
	CartProdSet() { }

	CartProdSet(const BaseSet1& b1, const BaseSet2& b2) : base1(b1), base2(b2) { }

	bool contains(const value_type& elem) const {
		Tuple1 t1;Tuple2 t2;
		std::tie(t1, t2) = split<Tuple1,Tuple2>(elem);

		return dfa::contains(base1, t1) && dfa::contains(base2, std::get<0>(t2));
	}

	const BaseSet1& getLeftBaseSet() const { return base1; }

	const BaseSet2& getRightBaseSet() const { return base2; }

	size_t size() const { return base1.size() * base2.size(); }

	bool bounded() const { return dfa::isBounded(base1) && dfa::isBounded(base2); }
};

/** 
 * Specialization for the case Ax(BxC)
 */
template <class Base1, class Base2, class Base3>
class CartProdSet<Base1,CartProdSet<Base2,Base3>> :
	public SymbolicSet< 
			typename TupleMerger<
				std::tuple<typename Base1::value_type>,
				typename CartProdSet<Base2,Base3>::value_type
			>::type > 
{

	Base1 base1;
	CartProdSet<Base2,Base3> base2;

public:

	typedef Base1					 BaseSet1;
	typedef CartProdSet<Base2,Base3> BaseSet2;

	typedef typename std::tuple<typename BaseSet1::value_type> Tuple1;
	typedef typename CartProdSet<Base2,Base3>::value_type Tuple2;

	typedef typename TupleMerger<Tuple1,Tuple2>::type value_type;

	typedef std::set<value_type> base_type;

	// Creates an empty cartesian-product
	CartProdSet() { }

	CartProdSet(const BaseSet1& b1, const Base2& b2, const Base3& b3) : 
		base1(b1), base2(CartProdSet<Base2,Base3>(b2, b3)) { }

	CartProdSet(const BaseSet1& b1, const BaseSet2& b2) : base1(b1), base2(b2) { }

	bool contains(const value_type& elem) const {
		Tuple1 t1;
		Tuple2 t2;
		std::tie(t1,t2) = split<Tuple1,Tuple2>(elem);
		return dfa::contains(base1, std::get<0>(t1)) && dfa::contains(base2, t2);
	}

	const BaseSet1& getLeftBaseSet() const { return base1; }

	const BaseSet2& getRightBaseSet() const { return base2; }

	base_type expand() const { 

		auto bt1 = dfa::expand(base1);
		auto bt2 = dfa::expand(base2);

		base_type res;
		for(auto& it1 : bt1) 
			for (auto& it2 : bt2) {
				res.insert( std::make_tuple(it1,std::get<0>(it2),std::get<1>(it2)) ); 	
			}

		return res;
	}

	size_t size() const { return base1.size() * base2.size(); }

	bool bounded() const { return dfa::isBounded(base1) && dfa::isBounded(base2); }
};


} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
