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

#include "insieme/simple_backend/utils/simple_backend_utils.h"

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace simple_backend {
namespace utils {

	using namespace core;

	bool isMainProgram(const ProgramPtr& program) {
		if (program->isMain()) {
			return true;
		}

		if (program->getEntryPoints().size() != static_cast<unsigned>(1)) {
			return false;
		}

		// construct the type of the main
		core::ASTBuilder builder(program->getNodeManager());
		const core::lang::BasicGenerator& basic = builder.getBasicGenerator();

		// type:   (int<4>, ref<array<array<char,1>,1>>) -> int<4>
		ConcreteIntTypeParamPtr one = builder.concreteIntTypeParam(1);
		vector<TypePtr> params = toVector<TypePtr>(basic.getInt4(),
				builder.refType(builder.arrayType(builder.arrayType(basic.getChar(), one), one)));

		FunctionTypePtr funPtr1 = builder.functionType(params, basic.getInt4());
		FunctionTypePtr funPtr2 = builder.functionType(TypeList(), basic.getInt4());
		FunctionTypePtr funPtr3 = builder.functionType(TypeList(), basic.getUnit());

		// check type
		const TypePtr& type = program->getEntryPoints()[0]->getType();
		return *type == *funPtr1 || *type == *funPtr2 || *type == *funPtr3;
	}


} // end namespace utils
} // end namespace simple_backend
} // end namespace insieme
