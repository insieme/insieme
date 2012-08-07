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

#include "insieme/utils/matrix.h"

#include "insieme/analysis/polyhedral/constraint.h"

namespace insieme { namespace analysis { namespace polyhedral {

using insieme::utils::Matrix;

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

	inline void readFromMatrix(const utils::Matrix<int>& coeffs) {
		if ( coeffs.empty() ) { return; }
		for_each(coeffs, [&](const typename utils::Matrix<int>::value_type& cur) { 
			this->append(cur); 
		});
	}
public:

	// Defines an iterator used to visit the Affine functions contained in this system
	template <class T, class IterT>
	class Iterator : public boost::random_access_iterator_helper<Iterator<T, IterT>, T> {
		IterT it, end;

	public:
		Iterator(const IterT& begin, const IterT& end): it(begin), end(end) { }

        inline T& operator*() const { return **it; }

        inline Iterator<T, IterT>& operator++() { ++it; return *this; }
		inline Iterator<T, IterT>& operator+=(size_t val) { it+=val; return *this; }

        inline bool operator==(const Iterator<T, IterT>& rhs) const { 
			return it == rhs.it;
		}

		inline const IterT& get() const { return it; }
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
		for_each(other.funcs, [&] (const AffineFunctionPtr& cur) { this->append(*cur); } );
		assert( other.funcs.size() == funcs.size() );
	}

	AffineSystem(const IterationVector& iterVec, const utils::Matrix<int>& coeffs) : 
		iterVec(iterVec) 
	{
		readFromMatrix(coeffs); 
	}

	AffineSystem(const IterationVector& iterVec, const std::vector<std::vector<int>>& coeffs) : 
		iterVec(iterVec) 
	{
		readFromMatrix( utils::Matrix<int>(coeffs) ); 
	}


	/**
	 * Returns the iteration vector on which the system is based
	 */
	inline const IterationVector& getIterationVector() const { return iterVec; }

	// Insert/appends a new AffineFunction to this system
	void insert(const iterator& pos, const AffineFunction& af);
	inline void append(const AffineFunction& af) { insert(end(), af); }

	// Insert/Append a new affine function taking the coefficients 
	inline void insert(const iterator& pos, const std::vector<int>& coeffs) {
		insert(pos, AffineFunction(iterVec, coeffs) );
	}

	inline void insert(const iterator& pos, const utils::Matrix<int>::Row& coeffs) {
		insert(pos, AffineFunction(iterVec, coeffs) );
	}

	inline void append(const std::vector<int>& coeffs) { insert(end(), coeffs); }
	inline void append(const utils::Matrix<int>::Row& coeffs) { insert(end(), coeffs); }

	// Removes rows from this affine system
	inline void remove(const iterator& iter) { funcs.erase( iter.get() ); }
	inline void remove(size_t pos) { funcs.erase( funcs.begin() + pos ); }
	
	inline void clear() { funcs.clear(); }

	inline void set(const std::vector<std::vector<int>>& coeffs) { 
		set( utils::Matrix<int>( coeffs ) );
	}

	inline void set(const utils::Matrix<int>& coeffs) { 
		// Clear the current matrix of coefficients 
		clear();
		for_each(coeffs, [&](const typename Matrix<int>::value_type& cur) { append(cur); });
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
	inline AffineFunction& operator[]( size_t n ) { 
		assert( n < funcs.size() && "Index out of array bounds" );
		return *funcs[n]; 
	}
	inline const AffineFunction& operator[]( size_t n ) const { 
		assert( n < funcs.size() && "Index out of array bounds" );
		return *funcs[n];
	}

	std::ostream& printTo(std::ostream& out) const;
};

typedef std::shared_ptr<AffineSystem> AffineSystemPtr;

utils::Matrix<int> extractFrom(const AffineSystem& sys);

std::vector<core::VariablePtr> getOrderedIteratorsFor(const AffineSystem& sched);


} } } // end insieme::analysis::polyhedral namespace 
