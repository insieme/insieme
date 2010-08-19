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

#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

#include <boost/type_traits/is_pointer.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/noncopyable.hpp>

using std::shared_ptr;
using std::vector;
using std::function;
using std::for_each;


template<typename T>
bool visitorTrueFilter(T) {
	return true;
}


template <typename T> class Visitor;


//template<typename T, typename boost::enable_if<boost::is_pointer<T>, int>::type = 0>
template<typename T>
class Visitable {
public:
	typedef shared_ptr<vector<T> > ChildList;

	virtual ChildList getChildren() const = 0;

protected:
	static ChildList makeChildList(const vector<T>& initial = vector<T>()) {
		return ChildList(new vector<T>(initial));
	}
	static ChildList makeChildList(const T& initialElement) {
		ChildList ret = makeChildList();
		ret->push_back(initialElement);
		return ret;
	}
};




template <typename T>
class Visitor : boost::noncopyable {

protected:
	const function<void(T)> task;
	const function<bool(T)> filter;



public:
	Visitor(const function<void(T)>& task, const function<bool(T)>& filter = &visitorTrueFilter<T>) : task(task), filter(filter) {};

	virtual void visit(T cur) const = 0;
};

template <typename T, typename RetT = void>
class DepthFirstVisitor : public Visitor<T> {
public:
	DepthFirstVisitor(const function<RetT(T)>& task, const function<bool(T)>& filter = &visitorTrueFilter<T>) : Visitor<T>(task, filter) {};

	virtual RetT visit(T cur) const {
		typename Visitable<T>::ChildList list = cur->getChildren();

		for_each(list->begin(), list->end(),
				[&](T cur) { this->visit(cur);
		});

		if (filter(cur)) {
			task(cur);
		}
	}
};


template <typename T, typename RetT = void>
class ChildVisitor : public Visitor<T> {
public:
	ChildVisitor(const function<void(T)>& task, const function<RetT(T)>& filter = &visitorTrueFilter<T>) : Visitor<T>(task, filter) {};

	virtual RetT visit(T cur) const {
		typename Visitable<T>::ChildList list = cur->getChildren();
		for_each(list->begin(), list->end(), [&](T cur) {
			if (this->filter(cur)) {
				this->task(cur);
			};
		});
	}
};

