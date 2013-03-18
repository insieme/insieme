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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS


#include "clang/AST/StmtVisitor.h"

#include <clang/AST/ExprCXX.h>
#include <clang/AST/Expr.h>

#include <iostream>
#include <vector>
namespace insieme {
namespace frontend { 
namespace utils {

namespace{

class temporariesVisitor : public clang::ConstStmtVisitor<temporariesVisitor, bool> {


	private:
		std::vector<const clang::CXXTemporary*>& tempList;

	public:
		temporariesVisitor (std::vector<const clang::CXXTemporary*>& list) 
			: tempList(list) {}


		bool 	Visit (const clang::Stmt *S){
			std::cout << " ************************* " << std::endl;

			if (llvm::isa<clang::CXXBindTemporaryExpr>(S))
				tempList.push_back(llvm::cast<clang::CXXBindTemporaryExpr>(S)->getTemporary ());

			for( clang::Stmt::const_child_iterator child_it = S->child_begin(); child_it!= S->child_end(); child_it++)
				Visit(*child_it);

			return false;
		}

		std::vector<const clang::CXXTemporary*>& lookTemporaries (const clang::Expr* start){

			this->Visit (llvm::cast<clang::Stmt>(start));
			return tempList;
		}

};


} //annonymous namespace

	/**
	 *  search in the inner tree for the used temporaries
	 */
	std::vector<const clang::CXXTemporary*> lookupTemporaries (const clang::Expr* innerExpr){

		std::vector<const clang::CXXTemporary*> temporaries;
		temporariesVisitor vis(temporaries);
		vis.lookTemporaries(innerExpr);

		return temporaries;
	}



} //namespace utils 
} //namespace frontend 
} //namespace insieme 
