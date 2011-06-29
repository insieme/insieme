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

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"
#include <vector>
#include <set>
#include <memory>

namespace insieme {
namespace analysis {

/** 
 * Class Ref represent a generic IR ref which can be either assigned or read. In this context 
 * a Ref can be either a scalar variable, an array or a vector (having a ref type), a struct/class
 * member or the return value of a call expression returning a ref. 
 */
struct Ref : public utils::Printable {
	// possible usage of a variable can be of three types: 
	// 	USE: the variable is being accessed, therefore the memory location is read and not modified 
	// 	DEF: the variable is being redefined (declaration and assignment), this means that the
	// 	     memory associated to that variable is being written 
	// 	UNKNOWN: the variable is being used as input parameter to a function which can either read
	// 	         or modify the value. UNKNWON type of usages can be refined through advanced dataflow
	// 	         analysis
	enum UseType { DEF, USE, UNKNOWN };

	// Points to the base expression: 
	// 	 this can be either a scalar variable, an array or a call to a function 
	core::ExpressionPtr baseExpr;

	// Define the use for this expression  
	UseType usage; 

	Ref(const core::ExpressionPtr& var, const UseType& usage = USE);

	std::ostream& printTo(std::ostream& out) const;

	const UseType& getUsage() const { return usage; }
};

// In the case of arrays (or vectors), we also store the list of expressions used to index each of the
// array dimensions
struct ArrayRef : public Ref { 
	
	std::vector<core::ExpressionPtr> idxExpr; 

	ArrayRef(const core::ExpressionPtr& arrayVar, const std::vector<core::ExpressionPtr>& idxExpr, const UseType& usage = USE) :
		Ref(arrayVar, usage), idxExpr(idxExpr) { }

	std::ostream& printTo(std::ostream& out) const;	
};

typedef std::shared_ptr<Ref> RefPtr; 

// Store the set of refs found by the visitor 
typedef std::set<RefPtr> RefSet;

// Main entry method, visits the IR starting from the root node collecting refs. The list of
// detected refs is returned to the caller. 
RefSet collectDefUse(const core::NodePtr& root);

} // end namespace analysis 
} // end namesapce insieme 
