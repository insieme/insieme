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

#include <iterator>
#include <stdexcept>
#include <memory>
#include <set>
#include <list>

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/constraint.h"
#include "insieme/core/ir_node.h"
#include "insieme/utils/printable.h"

#include "boost/operators.hpp"
#include "boost/optional.hpp"
#include "boost/mpl/or.hpp"

namespace insieme {
namespace analysis {
namespace poly {

/**************************************************************************************************
 * IterationDomain: the iteration domain represent the domain on which a statement is valid.
 * Therefore it is a represented by a set of constraints (ConstraintCombiner). However, the
 * iteration domain also allows the creation of empty and universe sets which are used to represent
 * statement which are not bound by any constraint
 **************************************************************************************************/

class IterationDomain : public utils::Printable {

	const IterationVector& iterVec;
	ConstraintCombinerPtr  constraint;
	bool empty;

public:
	IterationDomain( const IterationVector& iterVec, bool empty=false) : 
		iterVec(iterVec), empty(empty) { }

	explicit IterationDomain( const ConstraintCombinerPtr& constraint ) : 
		iterVec( extractIterationVector(constraint) ), constraint(constraint), empty(false) { }

	IterationDomain( const IterationVector& iv, const IterationDomain& otherDom) : 
		iterVec(iv), constraint( poly::cloneConstraint(iv, otherDom.constraint ) ), empty(false) { }
	
	const IterationVector& getIterationVector() const { return iterVec; }

	const ConstraintCombinerPtr& getConstraint() const { return constraint; }

	bool isUniverse() const { return !empty && !static_cast<bool>(constraint); }

	bool isEmpty() const { return empty; }

	IterationDomain& operator&=(const IterationDomain& other) {
		assert(iterVec == other.iterVec);
		constraint = constraint and other.constraint;
		return *this;
	}

	std::ostream& printTo(std::ostream& out) const;
};

IterationDomain operator&&(const IterationDomain& lhs, const IterationDomain& rhs);
IterationDomain operator||(const IterationDomain& lhs, const IterationDomain& rhs);
IterationDomain operator!(const IterationDomain& other);

struct AffineSystem : public utils::Printable {
	
	typedef std::list<AffineFunction> AffineList;

	AffineSystem(const IterationVector& iterVec) : iterVec(iterVec) { }	
	AffineSystem(const AffineSystem& other) : iterVec(other.iterVec) { cloneRows(other.funcs); }

	inline const IterationVector& getIterationVector() const { return iterVec; }

	void appendRow(const AffineFunction& af);

	inline size_t size() const { return funcs.size(); }

	std::ostream& printTo(std::ostream& out) const;

	AffineList::const_iterator begin() const { return funcs.begin(); }

	AffineList::const_iterator end() const { return funcs.end(); }

private:	
	void cloneRows(const AffineList&);

	const IterationVector& iterVec; 
	AffineList funcs;
};

typedef std::shared_ptr<AffineSystem> AffineSystemPtr;

//*****************************************************************************************************
// ScatteringFunction: A scattering function represent the order of execution of statements inside a
// SCoP
//*****************************************************************************************************
struct ScatteringFunction : public AffineSystem {

	ScatteringFunction(const IterationVector& iterVec) : AffineSystem(iterVec) { }
	ScatteringFunction(const ScatteringFunction& other) : AffineSystem(other) { }

};

typedef std::shared_ptr<ScatteringFunction> ScatteringFunctionPtr;

} // end poly namespace
} // end analysis namespace
} // end insieme namespace 




