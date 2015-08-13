/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include <iomanip>

#include <boost/operators.hpp>
#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

#define PRINT_CELL_WIDTH 3

	/**
	 * The Matrix class represent the abstraction of a two-dimensional array. The main characteristics
	 * of the abstraction is to provide capabilities to swap columns and rows of the matrix in O(1).
	 */
	template <class T>
	class Matrix : public utils::Printable {
		// Iterator class used to iterate both through the rows and the elements within a row of the
		// matrix.
		template <class Ty, class IterT>
		class Iterator : public boost::random_access_iterator_helper<Iterator<Ty, IterT>, Ty, std::ptrdiff_t> {
			Ty* begin;
			IterT it;

		  public:
			typedef Iterator<Ty, IterT> self;
			typedef std::ptrdiff_t Distance;

			Iterator(Ty* row, const IterT& begin) : begin(row), it(begin) {}

			Ty& operator*() const {
				return begin[*it];
			}

			self& operator++() {
				++it;
				return *this;
			}
			self& operator+=(size_t val) {
				it += val;
				return *this;
			}

			self& operator--() {
				--it;
				return *this;
			}
			self& operator-=(size_t val) {
				it -= val;
				return *this;
			}

			bool operator==(const self& rhs) const {
				return it == rhs.it;
			}

			friend Distance operator-(const self& lhs, const self& rhs) {
				return lhs.it - rhs.it;
			}
		};

	  public:
		typedef std::vector<size_t> IndexVect;

		class Row;
		typedef std::vector<Row> RowVect;

		/**
		 * The Row abstraction represents a row of the matrix. It keeps a pointer to the beginning and
		 * end of each row, it always refer the memory owned by the Matrix class therefore after the
		 * matrix object is being destroied all the corresponding Row objets are invalidated.
		 */
		class Row : public utils::Printable {
			IndexVect* mColIdx;
			T *mBegin, *mEnd;
			size_t mSize;

			typedef Iterator<T, IndexVect::const_iterator> iterator;
			typedef Iterator<const T, IndexVect::const_iterator> const_iterator;

		  public:
			// Private constructor because only Matrix can create instances of this class
			Row(IndexVect* colIdx = NULL, T* begin = NULL, T* end = NULL) : mColIdx(colIdx), mBegin(begin), mEnd(end), mSize(std::distance(mBegin, mEnd)) {}

			friend class utils::Matrix<T>;

			typedef T value_type;

			// Allows std::vector to construct objects of type row even with a private constructor
			template <class T1, class... Args>
			friend void std::_Construct(T1*, Args&&...);

			inline T& operator[](const size_t& pos) {
				assert_true(mColIdx && mSize > pos) << "Index out of bounds";
				return mBegin[(*mColIdx)[pos]];
			}

			inline const T& operator[](const size_t& pos) const {
				assert_true(mColIdx && mSize > pos) << "Index out of bounds";
				return mBegin[(*mColIdx)[pos]];
			}

			inline Row& operator=(const std::vector<T>& coeffs) {
				assert_eq(coeffs.size(), mSize) << "Index out of bounds";
				std::copy(coeffs.begin(), coeffs.end(), begin());
				return *this;
			}

			inline iterator begin() {
				return iterator(mBegin, mColIdx->begin());
			}
			inline iterator end() {
				return iterator(mBegin, mColIdx->end());
			}

			inline const_iterator begin() const {
				return const_iterator(mBegin, mColIdx->begin());
			}
			inline const_iterator end() const {
				return const_iterator(mBegin, mColIdx->end());
			}

			std::ostream& printTo(std::ostream& out) const {
				return out << join(" ", begin(), end(), [&](std::ostream& jout, const T& cur) { jout << std::setw(PRINT_CELL_WIDTH) << cur; });
			}

			inline size_t size() const {
				return mSize;
			}
		};

		typedef Row value_type;

		typedef Iterator<Row, IndexVect::const_iterator> iterator;
		typedef Iterator<const Row, IndexVect::const_iterator> const_iterator;

		/**
		 * Creates an empty matrix of size rows x cols. The cells of the matrix are initialized to 0
		 * when the init flas is set to true.
		 */
		Matrix(size_t rows, size_t cols, bool init = true, const T& initVal = T())
		    : mRawData(new T[rows * cols]), mRows(rows), mCols(cols), mRowVect(rows), mColIdx(cols), mRowIdx(rows) {
			if(init) { std::fill(mRawData, mRawData + (rows * cols), initVal); }
			initIndeces();
			updateVects();
		}

		Matrix(const std::vector<std::vector<T>>& coeffs) {
			if(coeffs.empty()) { return; }

			mRows = coeffs.size();
			mCols = coeffs.front().size();

			mRawData = new T[mRows * mCols];
			mRowVect = RowVect(mRows);
			mColIdx = IndexVect(mCols);
			mRowIdx = IndexVect(mRows);

			initIndeces();
			updateVects();

			for(size_t pos = 0; pos < mRows; ++pos) {
				assert_eq(coeffs[pos].size(), mCols);
				std::copy(coeffs[pos].begin(), coeffs[pos].end(), (*this)[pos].begin());
			}
		}

		Matrix(const Matrix<T>& other)
		    : mRawData(new T[other.mRows * other.mCols]), mRows(other.mRows), mCols(other.mCols), mRowVect(other.mRows), mColIdx(other.mColIdx),
		      mRowIdx(other.mRowIdx) {
			memcpy(mRawData, other.mRawData, mRows * mCols * sizeof(T));
			updateVects();
		}

		Matrix<T>::Row& operator[](size_t pos) {
			assert_lt(pos, mRows);
			return mRowVect[mRowIdx[pos]];
		}

		const Matrix<T>::Row& operator[](size_t pos) const {
			assert_lt(pos, mRows);
			return mRowVect[mRowIdx[pos]];
		}

		void swapRows(size_t i, size_t j) {
			assert_true(i < mRows && j < mRows) << "Rows indeces out of bounds";
			size_t tmp = mRowIdx[i];
			mRowIdx[i] = mRowIdx[j];
			mRowIdx[j] = tmp;
		}

		void swapCols(size_t i, size_t j) {
			assert_true(i < mCols && j < mCols) << "Columns indeces out of bounds";
			size_t tmp = mColIdx[i];
			mColIdx[i] = mColIdx[j];
			mColIdx[j] = tmp;
		}

		inline iterator begin() {
			return iterator(&mRowVect.front(), mRowIdx.begin());
		}
		inline iterator end() {
			return iterator(&mRowVect.front(), mRowIdx.end());
		}

		inline const_iterator begin() const {
			return const_iterator(&mRowVect.front(), mRowIdx.begin());
		}
		inline const_iterator end() const {
			return const_iterator(&mRowVect.front(), mRowIdx.end());
		}

		inline Row& front() {
			return mRowVect.front();
		}
		inline const Row& front() const {
			return mRowVect.front();
		}

		size_t rows() const {
			return mRows;
		}
		size_t cols() const {
			return mCols;
		}

		bool empty() const {
			return mCols * mRows == 0;
		}

		std::ostream& printTo(std::ostream& out) const {
			out << "mat(" << mRows << ", " << mCols << ") - {\n  ";
			out << join("\n  ", begin(), end(), [&](std::ostream& jout, const Row& cur) { jout << cur; });
			return out << "\n}" << std::endl;
		}

		template <class TT>
		bool operator==(const Matrix<TT>& other) const {
			if(typeid(T) != typeid(TT)) { return false; }
			if(mRows != other.mRows || mCols != other.mCols) { return false; }

			bool isEqual = true;
			for(size_t pos = 0; pos < mRows && isEqual; ++pos) {
				isEqual = std::equal((*this)[pos].begin(), (*this)[pos].end(), other[pos].begin());
			}

			return isEqual;
		}

		~Matrix() {
			delete[] mRawData;
		}

	  private:
		void updateVects() {
			T* ptr = mRawData;
			for(size_t i = 0; i < mRows; i++, ptr += mCols) {
				mRowVect[i] = Row(&mColIdx, ptr, ptr + mCols);
			}

			assert_eq(mRawData + mRows * mCols, ptr);
		}

		void initIndeces() {
			auto initializer = [&](IndexVect::iterator begin, const IndexVect::iterator& end) {
				for(size_t pos = 0; begin != end; ++begin, ++pos) {
					*begin = pos;
				}
			};

			initializer(mColIdx.begin(), mColIdx.end());
			initializer(mRowIdx.begin(), mRowIdx.end());
		}

		/**
		 * Pointer to the raw memory hosting the matrix
		 */
		T* mRawData;

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

	template <class T>
	Matrix<T> makeIdentity(size_t size) {
		Matrix<T> ret(size, size);

		int* ptr = &ret[0][0];
		for(size_t pos = 0; pos < size; ++pos) {
			ptr[(size + 1) * pos] = 1;
		}

		return ret;
	}

	template <class T>
	Matrix<T> operator*(const Matrix<T>& lhs, const Matrix<T>& rhs) {
		assert_eq(lhs.cols(), rhs.rows()) << "Matrix not allowed, size mismatch";

		Matrix<T> ret(lhs.rows(), rhs.cols());

		for(size_t i = 0; i < lhs.rows(); ++i) {
			for(size_t j = 0; j < rhs.cols(); ++j) {
				for(size_t k = 0; k < lhs.cols(); ++k) {
					ret[i][j] += lhs[i][k] * rhs[k][j];
				}
			}
		}

		return ret;
	}

	namespace {

		template <class T, class Op>
		Matrix<T> matOp(const Matrix<T>& lhs, const Matrix<T>& rhs, const Op& op) {
			Matrix<int> ret(lhs.rows(), lhs.cols());
			for(size_t i = 0; i < lhs.rows(); ++i) {
				std::transform(lhs[i].begin(), lhs[i].end(), rhs[i].begin(), ret[i].begin(), op);
			}
			return ret;
		}

	} // end anoynous namespace

	template <class T>
	Matrix<T> operator+(const Matrix<T>& lhs, const Matrix<T>& rhs) {
		assert_true(lhs.rows() == rhs.rows() && lhs.cols() == rhs.cols()) << "Matrix sum not allowed, size mismatch";

		return matOp(lhs, rhs, std::plus<T>());
	}

	template <class T>
	Matrix<T> operator-(const Matrix<T>& lhs, const Matrix<T>& rhs) {
		assert_true(lhs.rows() == rhs.rows() && lhs.cols() == rhs.cols()) << "Matrix diff not allowed, size mismatch";

		return matOp(lhs, rhs, std::minus<T>());
	}

} // end utils namespace
} // end insieme namespace
