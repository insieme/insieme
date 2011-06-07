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

#include <utility>
#include <iterator>

template<
	typename ITypeA,
	typename ITypeB,
	typename ValueTypeA = typename std::iterator_traits<ITypeA>::value_type,
	typename ValueTypeB = typename std::iterator_traits<ITypeB>::value_type>
class IteratorParentType : public std::iterator<std::input_iterator_tag, std::pair<ValueTypeA, ValueTypeB> > { };

// todo inherit from iterator traits
template<typename ITypeA, typename ITypeB>
class paired_iterator : public IteratorParentType<ITypeA, ITypeB> {
	ITypeA a;
	ITypeB b;

	typedef typename IteratorParentType<ITypeA, ITypeB>::value_type ValueType;

	ValueType cur;

public:
	paired_iterator(ITypeA a, ITypeB b) : a(a), b(b) { }

	ValueType& operator*() {
		cur = std::make_pair(*a, *b);
		return cur;
	}

	ValueType* operator->() {
		cur = std::make_pair(*a, *b);
		return &cur;
	}

	paired_iterator& operator++() {
		++a;
		++b;
		return *this;
	}

	paired_iterator operator++(int) {
		paired_iterator ret = *this;
		++this;
		return ret;
	}

	bool operator==(const paired_iterator& rhs) {
		return (a == rhs.a) && (b == rhs.b);
	}
	
	bool operator!=(const paired_iterator& rhs) {
		return (a != rhs.a) || (b != rhs.b);
	}
};

template<typename A, typename B>
paired_iterator<A, B> make_paired_iterator(A a, B b) {
	return paired_iterator<A,B>(a, b);
}


// -----------------------------------------------------------------------
//                 Cartesian Product Iterator
// -----------------------------------------------------------------------

// todo inherit from iterator traits
template<typename ITypeA, typename ITypeB>
class product_iterator : public IteratorParentType<ITypeA, ITypeB> {
	ITypeA beginA;
	ITypeA endA;
	ITypeB endB;
	ITypeA a;
	ITypeB b;

	typedef typename IteratorParentType<ITypeA, ITypeB>::value_type ValueType;

	/**
	 * A instance of the currently referenced element.
	 */
	ValueType cur;

public:
	product_iterator(ITypeA beginA, ITypeA endA, ITypeB beginB, ITypeB endB)
		: beginA(beginA), endA(endA), endB(endB), a(beginA), b(beginB) { }

	ValueType& operator*() {
		cur = std::make_pair(*a, *b);
		return cur;
	}

	ValueType* operator->() {
		cur = std::make_pair(*a, *b);
		return &cur;
	}

	product_iterator& operator++() {
		++a;
		if (a == endA) {
			++b;
			if (b != endB) {
				a= beginA;
			}
		}
		return *this;
	}

	product_iterator operator++(int) {
		product_iterator ret = *this;
		++this;
		return ret;
	}

	bool operator==(const product_iterator& rhs) {
		return (a == rhs.a) && (b == rhs.b);
	}

	bool operator!=(const product_iterator& rhs) {
		return (a != rhs.a) || (b != rhs.b);
	}
};

template<
	typename ContainerA,
	typename ContainerB,
	typename IterA = typename ContainerA::const_iterator,
	typename IterB = typename ContainerB::const_iterator,
	typename ResIter = product_iterator<IterA, IterB>
>
std::pair<ResIter, ResIter> make_product_range(const ContainerA& first, const ContainerB& second) {
	return std::make_pair(
			ResIter(first.begin(), first.end(), second.begin(), second.end()),
			ResIter(first.end(), first.end(), second.end(), second.end())
	);
}
