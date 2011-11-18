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

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/utils/printable.h"

#include <iomanip>

namespace insieme {
namespace analysis {
namespace poly {

template <class T>
class Matrix : public utils::Printable {
public:

	typedef T value_type;

	typedef std::vector<size_t> IndexVect;

	class Row;
	typedef std::vector<Row>	RowVect;

	class Row : public utils::Printable {
		IndexVect* colIdx;
		T* begin, *end;

	public:
		Row(IndexVect* colIdx=NULL, T* begin = NULL, T* end=NULL) : 
			colIdx(colIdx), begin(begin), end(end) { }

		T& operator[](const size_t& pos) { 
			assert( colIdx && static_cast<size_t>(std::distance(begin,end)) > pos );
			return begin[ (*colIdx)[pos] ];
		}

		const T& operator[](const size_t& pos) const { 
			assert( colIdx && static_cast<size_t>(std::distance(begin,end)) > pos );
			return begin[ (*colIdx)[pos] ];
		}

		Row& operator=( const std::vector<T>& coeffs ) {
			assert( coeffs.size() == static_cast<size_t>(std::distance(begin,end)) );
			for(size_t pos=0; pos<coeffs.size(); ++pos) {
				begin[ (*colIdx)[pos] ] = coeffs[pos];
			}
			return *this;
		}

		std::ostream& printTo(std::ostream& out) const { 
			size_t size = std::distance(begin,end);
			for(size_t c=0; c<size; ++c) {
				out << std::setw(3) << (*this)[c];
				if (c != size-1) { out << ", "; }
			}
			return out;
		}
	};

	Matrix(size_t rows, size_t cols) : 
		mRawData(new T[rows*cols]), mRows(rows), mCols(cols), mRowVect(rows), 
		mColIdx(cols), mRowIdx(rows)
	{
		memset(mRawData, 0, rows*cols*sizeof(T));

		auto initializer = [&] (IndexVect::iterator begin, const IndexVect::iterator& end) {
			size_t pos=0;
			for(;begin!=end;++begin, ++pos) { *begin = pos; }
		};
		
		initializer(mColIdx.begin(), mColIdx.end());
		initializer(mRowIdx.begin(), mRowIdx.end());
		
		updateVects();
	}

	Matrix(const Matrix<T>& other) : 
		mRawData( new T[other.mRows*other.mCols] ), mRows(other.mRows), 
		mCols(other.mCols), mRowVect(other.mRows), 
		mColIdx(other.mColIdx), mRowIdx(other.mRowIdx)
	{
		memcpy(mRawData, other.mRawData, mRows*mCols*sizeof(T) );
		updateVects();
	}

	Matrix<T>::Row& operator[](size_t pos) {
		assert( pos < mRows );
		return mRowVect[ mRowIdx[pos] ];
	}

	const Matrix<T>::Row& operator[](size_t pos) const {
		assert( pos < mRows );
		return mRowVect[ mRowIdx[pos] ];
	}

	void swapRows(size_t i, size_t j) { 
		assert(i < mRows && j < mRows && "Rows indeces out of bounds");
		size_t tmp = mRowIdx[i];
		mRowIdx[i] = j;
		mRowIdx[j] = tmp;
	}

	void swapCols(size_t i, size_t j) { 
		assert(i < mCols && j < mCols && "Columns indeces out of bounds");
		size_t tmp = mColIdx[i];
		mColIdx[i] = j;
		mColIdx[j] = tmp;
	}

	size_t rows() const { return mRows; }
	size_t cols() const { return mCols; }

	bool empty() const { return mCols*mRows == 0; }

	std::ostream& printTo(std::ostream& out) const { 
		out << "mat(" << mRows << ", " << mCols << ") - {\n";
		for (size_t r=0; r<mRows; ++r) {
			out << "  " << (*this)[r];
			if (r != mCols-1) { out << "\n"; }
		}
		return out << "\n}" << std::endl;
	}

	~Matrix() { delete[] mRawData; }

private:
	
	void updateVects() {
		
		T* ptr=mRawData;
		for(size_t i=0; i<mRows; i++, ptr+=mCols )          
			mRowVect[i] = Row( &mColIdx, ptr, ptr+mCols );

		assert(mRawData + mRows*mCols == ptr);
	}

	/**
	 * Pointer to the raw memory hosting the matrix
	 */
	T* 	mRawData;	

	/**
	 * Number of rows and cols of this matrix 
	 */
	size_t mRows, mCols;

	/**
	 * Vector of rows pointing to begin/end of every raw in the matrix 
	 */
	RowVect mRowVect;

	/**
	 * row and col indeces utilized to address elements of the matrix
	 */
	IndexVect mColIdx, mRowIdx;

}; 

/**
 * Unimodular transformations: a transformation is represented by a matrix
 */
class UnimodularMatrix : public Matrix<int> {

public:
	UnimodularMatrix( size_t rows, size_t cols ) : Matrix<int>(rows, cols) { 
		assert(!empty() && "Creation of empty Unimodular matrix is not allowed"); 
	}

	UnimodularMatrix( const Matrix<int>& mat ) : Matrix<int>(mat) { }

};

// Creates an identity matrix 
UnimodularMatrix makeIdentity(size_t size) { 

	Matrix<int> coeffs(size, size);

	int* ptr = &coeffs[0][0];
	for(size_t pos = 0; pos < size; ++pos) {
		ptr[ (size+1)*pos ] = 1;
	}

	return UnimodularMatrix( coeffs );
}

// Creates a matrix for loop interchange 
UnimodularMatrix makeInterchangeMatrix(size_t size, size_t src, size_t dest) {

	Matrix<int>&& m = makeIdentity(size);
	m.swapRows(src, dest);
	return m;

}

UnimodularMatrix makeInterchangeMatrix(const IterationVector& iterVec, const core::VariablePtr& src, const core::VariablePtr& dest) {

	int srcIdx = iterVec.getIdx( poly::Iterator(src) );
	int destIdx = iterVec.getIdx( poly::Iterator(dest) );
	assert( srcIdx != -1 && destIdx != -1 && srcIdx != destIdx && "Interchange not valid");
	return makeInterchangeMatrix( iterVec.size(), srcIdx, destIdx);
}


} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace

