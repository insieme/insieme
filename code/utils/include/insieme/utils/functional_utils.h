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

#include <type_traits>

#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_pointer.hpp>
#include <boost/type_traits/remove_pointer.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/utility/enable_if.hpp>

struct empty {};

template<typename T>
struct id : public std::unary_function<const T&, const T&> {
	const T& operator()(const T& element) const { return element; }
};


template<typename PointerType>
struct deref: public std::unary_function<const PointerType&, const typename PointerType::element_type&> {
	const typename PointerType::element_type& operator()(const PointerType& ptr) const {
		return *ptr;
	}
};

template<typename T>
struct deref<T*> : public std::unary_function<T*, T&> {
	T& operator()(T* ptr) const {
		return *ptr;
	}
};

template<typename T>
struct deref<const T*> : public std::unary_function<const T*, const T&> {
	const T& operator()(const T* ptr) const {
		return *ptr;
	}
};

/**
 * Compares two pointers to verify whether they are referencing the same
 * element. Two null pointers are considered equivalent. A null pointer
 * is not equivalent to any non-null pointer. Two non-null pointers are
 * only equivalent if the references objects are equivalent.
 *
 * @param a the pointer to the first element to be compared
 * @param b the pointer to the second element to be compared
 */
template<typename PointerType>
inline bool equalTarget(const PointerType& a, const PointerType& b) {
	return a == b || (a && b && *a == *b);
}

/**
 * This utility struct definition defines a predicate comparing two pointers
 * based on the value they are pointing to.
 *
 * @tparam PointerType the type of pointer to be compared
 */
template<typename PointerType>
struct equal_target : public std::binary_function<const PointerType&, const PointerType&, bool> {
	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const PointerType& x, const PointerType& y) const {
		return equalTarget(x, y);
	}
};

/**
 * This utility struct definition defines a predicate comparing two pointers
 * based on the value they are pointing to (operator <).
 *
 * @tparam PointerType the type of pointer to be compared
 */
template<typename PointerType>
struct compare_target : public std::binary_function<const PointerType&, const PointerType&, bool> {
	/**
	 * Performs the actual comparison by using the operator< of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const PointerType& x, const PointerType& y) const {
		return *x < *y;
	}
};



/**
 * This utility struct defines the function used to compute hash codes for pointers.
 * Thereby, the hash code is not computed using the pointer themselves. Instead, the
 * target they are pointing to is used to compute the value. In case the pointer is null,
 * 0 is returned as a hash value.
 *
 * @tparam PointerType the type of the pointer to be hashed
 */
template<typename PointerType, typename Enabled = void>
struct hash_target; // { /* default is not working */ };

/**
 * This partial template specialization of the hash_target struct is handling
 * real pointers.
 */
template<typename PointerType>
struct hash_target<PointerType, typename boost::enable_if<boost::is_pointer<PointerType>>::type> {

	/**
	 * Derives the element type be removing the pointer extension.
	 */
	typedef typename boost::remove_pointer<PointerType>::type ElementType;

	/**
	 * This function is used to compute the hash of the actual target.
	 */
	boost::hash<ElementType> hasher;

	/**
	 * Explicit Default constructor required by VC.
	 */
	hash_target() : hasher() {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const PointerType p) const {
		if (p) {
			return hasher(*p);
		}
		return 0;
	}
};

/**
 * This partial template specialization of the hash_target struct is handling
 * smart pointers.
 */
template<typename PointerType>
struct hash_target<PointerType, typename boost::disable_if<boost::is_pointer<PointerType>>::type> {

	/**
	 * Obtains the element type from the smart pointer.
	 */
	typedef typename PointerType::element_type ElementType;

	/**
	 * This function is used to compute the hash of the actual target.
	 */
	boost::hash<ElementType> hasher;

	/**
	 * Explicit Default constructor required by VC.
	 */
	hash_target() : hasher() {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const PointerType p) const {
		if (p) {
			return hasher(*p);
		}
		return 0;
	}
};

// -------------------- Filter Functions ----------------------------
// Filters are functors accepting a certain set of arguments and returning
// a boolean indicating true (accepted) or false (rejected).
// ------------------------------------------------------------------

/**
 * A (generic) functor accepting representing a filter accepting any value.
 */
template<typename ... T>
struct AcceptAll {
	bool operator()(T...args) const { return true; }
};

/**
 * A (generic) functor accepting representing a filter rejecting any value.
 */
template<typename ... T>
struct RejectAll {
	bool operator()(T...args) const { return false; }
};


// -------------------- Member function wrapper ---------------------
//  A functor wrapping member function calls to known objects.
// ------------------------------------------------------------------

/**
 * A functor combining an object pointer and a member function pointer
 * and invoking the member function on the given object when being
 * invoked.
 *
 * Instances of this functor can be used to bridge the gap between
 * std::function instances and member function pointers.
 *
 * WARNING: Instances of this functor are only valid as long as the
 * underlying object is still alive. The functor will not keep it alive.
 */
template<typename C, typename R, typename ... A>
struct member_function {
	typedef R (C::* member_function_ptr)( A ... );
	typedef R (* pointer_to_member_function)(C*, A...);

	/**
	 * An extension of GCC allows to obtain a function pointer to the actual
	 * implementation of a member function. This way, the overhead of walking through
	 * virtual tables is only introduced during construction
	 *
	 * See:
	 * http://gcc.gnu.org/onlinedocs/gcc/Bound-member-functions.html#Bound-member-functions
	 *
	 * NOTE: this optimization was disabled since it is looking up the virtual function
	 * table based on the static type of the pointer. Hence, resolving the real type
	 * of a this pointer within a abstract super class was not supported.
	 */

	C* object;

	member_function_ptr fun;  // member function version

	member_function(C& object, const member_function_ptr& member)
		: object(&object), fun(member) {}

	R operator()(A...args) const {
		return (object->*fun)(args...);
	}

};

// the same as above, for const member functions
template<typename C, typename R, typename ... A>
struct member_function_const {
	typedef R (C::* member_function_ptr)( A ... ) const;
	typedef R (* pointer_to_member_function)(const C*, A...);

	const C* object;

	member_function_ptr fun;  // member function version

	member_function_const(const C& object, const member_function_ptr& member)
		: object(&object), fun(member) {}

	R operator()(A...args) const {
		return (object->*fun)(args...);
	}

};


/**
 * A utility factory method reducing the amount of code required to generate
 * a member function functor by eliminating the requirement of specifying all
 * the type parameters.
 *
 * @tparam C the class the member function is defined for
 * @tparam O the class of the object representing the target
 * @tparam R the result produced by the member function
 * @tparam A the argument types of the member function
 * @param object the object to be bound to
 * @param fun the member function pointer to be bound
 * @return a member function functor invoking the given member function on the
 * 		given object upon invocation
 */
template<typename C, typename O, typename R, typename ... A>
typename boost::enable_if<boost::is_base_of<C,O>,member_function<C,R,A...>>::type
fun(O& object, R (C::* fun)( A ... )) {
	return member_function<C,R,A...>(object, fun);
}
template<typename C, typename O, typename R, typename ... A>
typename boost::enable_if<boost::is_base_of<C,O>,member_function_const<C,R,A...>>::type
fun(const O& object, R (C::* fun)( A ... ) const) {
	return member_function_const<C,R,A...>(object, fun);
}

template<typename T>
struct member_function_trait;

template<typename C, typename R, typename ... A>
struct member_function_trait<R(C::*)(A...)> {
	typedef member_function<C,R,A...> type;
};

template<typename C, typename R, typename ... A>
struct member_function_trait<R(C::*)(A...) const> {
	typedef member_function_const<C,R,A...> type;
};

// -------------------- Type List traits ----------------------------


template <typename ... Ts>
struct type_list;

template<>
struct type_list<> {
	BOOST_STATIC_CONSTANT(bool, empty=true);
	BOOST_STATIC_CONSTANT(unsigned, length=0);
};

template<typename H, typename ... R>
struct type_list<H,R...> {
	BOOST_STATIC_CONSTANT(bool, empty=false);
	BOOST_STATIC_CONSTANT(unsigned, length=type_list<R...>::length + 1);
	typedef H head;
	typedef type_list<R...> rest;
};

template <typename ... Ts>
struct size_of;

template <typename ... Ts>
struct size_of<type_list<Ts...>> {
	enum { value = sizeof...(Ts) };
};

template<unsigned pos, typename L>
struct type_at;

template<typename H, typename ...R>
struct type_at<0, type_list<H,R...>> {
	typedef H type;
};

template<unsigned pos, typename H, typename ...R>
struct type_at<pos, type_list<H,R...>> {
	typedef typename type_at<pos-1, type_list<R...>>::type type;
};

template<typename L>
struct cons;

template<typename H, typename ...R>
struct cons<type_list<H,R...>> {
	typedef H head;
	typedef type_list<R...> rest;
};

// Concatenation of type_lists
template <class ... T>
struct concat;

// matches any number of typelists 
template <class... L1, class... Tail>
struct concat<type_list<L1...>,Tail...> {
	typedef typename concat<type_list<L1...>,typename concat<Tail...>::type>::type type;
};

// matches exactly 2 type lists
template <class... L1, class ... L2>
struct concat<type_list<L1...>,type_list<L2...>> {
	typedef type_list<L1...,L2...> type;
};

// matches empty typelists or single typelists
template <class... L>
struct concat<type_list<L...>> {
	typedef type_list<L...> type;
};


// -------------------- Function Traits for Lambdas ----------------------------
//
// see: http://stackoverflow.com/questions/2611357/lambda-traits-inconsistency-across-c0x-compilers
// see: boost function_traits.hpp (which unfortunatelly only work for function pointer, not member function pointer.

namespace detail {

	template<typename Function> struct lambda_traits_helper { };

	// get rid of const modifier
	template<typename T>
	struct lambda_traits_helper<const T> : public lambda_traits_helper<T> {};

	// get rid of pointers
	template<typename T>
	struct lambda_traits_helper<T*> : public lambda_traits_helper<T> {};

	// handle class of member function pointers
	template<typename R, typename C, typename ... A>
	struct lambda_traits_helper<R(C::*)(A...)> : public lambda_traits_helper<R(*)(A...)> {
		typedef C class_type;
	};

	// get rid of const modifier
	template<typename R, typename C, typename ... A>
	struct lambda_traits_helper<R(C::*)(A...) const> : public lambda_traits_helper<R(C::*)(A...)> {};

	template<typename R>
	struct lambda_traits_helper<R(void)>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 0);
	  typedef R result_type;
	  typedef type_list<> argument_types;
	};

	template<typename R, typename T1>
	struct lambda_traits_helper<R(T1)>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 1);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T1 argument_type;
	  typedef type_list<T1> argument_types;
	};

	template<typename R, typename T1, typename T2>
	struct lambda_traits_helper<R(T1, T2)>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 2);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T2 arg2_type;
	  typedef T1 first_argument_type;
	  typedef T2 second_argument_type;
	  typedef type_list<T1,T2> argument_types;
	};

	template<typename R, typename T1, typename T2, typename T3>
	struct lambda_traits_helper<R(T1, T2, T3)>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 3);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T2 arg2_type;
	  typedef T3 arg3_type;
	  typedef type_list<T1,T2,T3> argument_types;
	};

	template <typename R, typename T1, typename T2, typename T3, typename ... A >
	struct lambda_traits_helper<R( T1, T2, T3, A ... )>  {
		BOOST_STATIC_CONSTANT(unsigned, arity = 3 + sizeof...(A));
		typedef R result_type;
		typedef T1 arg1_type;
	    typedef T2 arg2_type;
	    typedef T3 arg3_type;
		typedef type_list<T1,T2,T3,A...> argument_types;
	};

} // end namespace detail


template <typename Lambda>
struct lambda_traits : public detail::lambda_traits_helper<decltype(&Lambda::operator())> { };

template<typename R, typename ... P>
struct lambda_traits<R(P...)> : public detail::lambda_traits_helper<R(P...)> { };

template<typename R, typename ... P>
struct lambda_traits<R(*)(P...)> : public lambda_traits<R(P...)> { };

template<typename R, typename ... P>
struct lambda_traits<R(* const)(P...)> : public lambda_traits<R(P...)> { };

template<typename R, typename C, typename ... P>
struct lambda_traits<R(C::*)(P...)> : public detail::lambda_traits_helper<R(C::*)(P...)> { };

template<typename R, typename C, typename ... P>
struct lambda_traits<R(C::* const)(P...)> : public lambda_traits<R(C::*)(P...)> { };


template<unsigned pos, typename ...R>
struct element_type;

template<typename H, typename ...R>
struct element_type<0,H,R...> {
	typedef H type;
};

template<unsigned pos, typename H, typename ...R>
struct element_type<pos,H,R...> {
	typedef typename element_type<pos-1,R...>::type type;
};

//==== FinalActions ===============================================================================
// A class which is utilized to invoke a sequence of statements (or action) when a block is exited. 
// Because there are situation where a block can be exited from multiple paths, this object will 
// make it sure thos actions will be invoked on every exit path. 
//
// Example:
//
// { 
// 	stream << "";
// 	if (x==0) {
// 		stream.close();
// 		return;
// 	}
// 	...
// 	stream.close()
// }
//
// Usage with FinalActions():
//
// { 
// 	FinalAction fa([&stream](){ stream.close(); });
// 	if(x==0)
// 		return;
// 	...
// 	return;
// }
//
// The action which is provided will be invoked when the block is exited, therefore in the
// destructor 
//=================================================================================================
class FinalActions {
	
	typedef std::function<void ()> Action;
	Action 	action;
	bool 	enabled;

public:
	FinalActions(const Action& action, bool enabled=true) : 
		action(action), enabled(enabled) { }
	
	bool isEnabled() const { return enabled; }

	void setEnabled(bool enabled) { this->enabled = enabled; }

	~FinalActions() { 
		if(isEnabled()) { action(); }
	}	
};

namespace insieme { namespace utils {

/**
 * Utility for composition of functions. Creates a bind object which models the lazy evaluation of a composition of
 * functions. For example f(g(h(X))) can be created and when the object is invoked with a value of X the h function 
 * is invoked, the result passed to g and so forth. 
 */
template <typename RetTy, typename... ArgTy>
struct FunctionComposition : public std::function<RetTy (ArgTy...)> {
	
	/**
	 * Constructor which allows to combine two functors (whatever their type is). This one is
	 * selected when the return type of g is the same as the argument type of f
	 */
	template <class T>
	FunctionComposition( const std::function<RetTy (T)>& f, const std::function<T (ArgTy...)>& g ) : 
		std::function<RetTy (ArgTy...)>( std::bind(f, std::bind(g, std::placeholders::_1)) ) { } 

	/**
	 * If f receive the output of g as a const ref, then this signature is selected
	 */
	template <class T>
	FunctionComposition( const std::function<RetTy (const T&)>& f, const std::function<T (ArgTy...)>& g ) : 
		std::function<RetTy (ArgTy...)>( std::bind(f, std::bind(g, std::placeholders::_1)) ) { } 

};

namespace detail {

template <class ...Classes>
struct last;

template <class Head1, class Head2, class ...Tail>
struct last<Head1, Head2, Tail...> {
	typedef typename last<Head2, Tail...>::value value;
};

template <class Head>
struct last<Head> {
	typedef typename lambda_traits<Head>::argument_type value;
};

} // end anonymous namespace 

template <class Func> 
std::function<typename lambda_traits<Func>::result_type (typename lambda_traits<Func>::argument_type)>
composeFunc(const Func& f) { return f; }

// compose multiple functions into a composition object
template <typename Func1, typename Func2, typename... Funcs>
FunctionComposition<
	typename lambda_traits<Func1>::result_type, 
	typename detail::last<Func2, Funcs...>::value
> composeFunc(const Func1& first, const Func2& second, const Funcs&... funcs) {
	return FunctionComposition<
			typename lambda_traits<Func1>::result_type,
			typename detail::last<Func2, Funcs...>::value
		>(composeFunc(first), composeFunc(second, funcs...));
}

} } // end insieme::utils namespce 
