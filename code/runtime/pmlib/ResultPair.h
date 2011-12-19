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

/*
 * ResultPair.h
 *
 *  Created on: Aug 8, 2011
 *      Author: eiter
 */

#ifndef RESULTPAIR_H_
#define RESULTPAIR_H_

template<class T, class V>
class ResultPair {
public:
	ResultPair(T first, V second);
	virtual ~ResultPair();
	bool operator>(const ResultPair<T, V> &c2) const;
	bool operator<=(const ResultPair<T, V> &c2) const;

	bool operator<(const ResultPair<T, V> &c2) const;
	bool operator>=(const ResultPair<T, V> &c2) const;

	/*
	 * Returns the first value of the result pair.
	 */
	T getFirst() const;

	/*
	 * Returns the second value of the result pair.
	 */
	V getSecond() const;

private:
	T first;
	V second;
};

template<class T, class V>
bool ResultPair<T, V>::operator>(const ResultPair<T, V> &c2) const {
	return getFirst() > c2.getFirst();
}

template<class T, class V>
bool ResultPair<T, V>::operator<=(const ResultPair<T, V> &c2) const {
	return getFirst() <= c2.getFirst();
}

template<class T, class V>
bool ResultPair<T, V>::operator<(const ResultPair<T, V> &c2) const{
	return getFirst() < c2.getFirst();
}

template<class T, class V>
bool ResultPair<T, V>::operator>=(const ResultPair<T, V> &c2) const{
	return getFirst() >= c2.getFirst();
}

template<class T, class V>
ResultPair<T, V>::ResultPair(T f, V s) : first(f), second(s){


}

template<class T, class V>
ResultPair<T, V>::~ResultPair() {
	// TODO Auto-generated destructor stub
}

template<class T, class V>
T ResultPair<T, V>::getFirst() const{
	return this->first;
}

template<class T, class V>
V ResultPair<T, V>::getSecond() const{
	return this->second;
}

#endif /* RESULTPAIR_H_ */
