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

#include "insieme/frontend/convert.h"
#include "insieme/frontend/stmt_converter.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/utils/ir_cast.h"

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/mpi/mpi_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/frontend/cpp/temporary_handler.h"

#include "clang/AST/StmtVisitor.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace insieme {
namespace frontend {
namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//			ConversionFactory
//---------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------------------------------------
//			ConversionFactory utility functions for CLANG STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------
ConversionFactory::CStmtConverter* ConversionFactory::makeStmtConvert(ConversionFactory& fact) {
	return new CStmtConverter(fact);
}

void ConversionFactory::cleanStmtConvert(StmtConverter* stmtConv) {
	delete stmtConv;
}

core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt* stmt) const {
	assert(currTU && "translation unit is null");
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return tryAggregateStmts(builder, stmtConv->Visit(const_cast<Stmt*>(stmt)));
}


//---------------------------------------------------------------------------------------------------------------------
//			CXXConversionFactory
//---------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------------------------------------
//			CXXConversionFactory utility functions for CXX STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------

CXXConversionFactory::CXXStmtConverter*
CXXConversionFactory::makeCXXStmtConvert(CXXConversionFactory& fact) {
	return new CXXConversionFactory::CXXStmtConverter(fact);
}

void CXXConversionFactory::cleanCXXStmtConvert(CXXStmtConverter* stmtConv) {
	delete stmtConv;
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
