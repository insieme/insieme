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

#include <gtest/gtest.h>

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/opencl/opencl_analysis.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/pointer.h"

#include "insieme/annotations/opencl/opencl_annotations.h"

namespace insieme {
namespace backend {
namespace opencl {

	using namespace insieme::annotations::opencl;

	namespace {
		void addAnnotations(const core::NodePtr& node, BaseAnnotation::AnnotationList& annos) {
			if(annos.empty()) return;
			// get old annotation list and append our annotations
			if(node->hasAnnotation(BaseAnnotation::KEY)) {
				auto& lst = node->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
				lst.insert(lst.end(), annos.begin(), annos.end());
			} else {
				// in this case we need to create a new one
				node->addAnnotation(std::make_shared<BaseAnnotation>(annos));
			}
		}
	}

	TEST(getUnderlyingType, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto uintTy = mgr.getLangBasic().getUInt4();
		EXPECT_EQ(toString(*uintTy), toString(*analysis::getUnderlyingType(builder.refType(builder.arrayType(uintTy)))));
	}

	TEST(isIndependentStmt, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		EXPECT_FALSE(analysis::isIndependentStmt(builder.getNoOp()));

		auto intTy = mgr.getLangBasic().getInt4();
		auto boolTy = mgr.getLangBasic().getBool();
		auto intLit = builder.literal(intTy, "4");
		auto intVar = builder.variable(intTy, 1);

		auto forStmt = builder.forStmt(intVar, intLit, intLit, intLit, intLit);
		EXPECT_FALSE(analysis::isIndependentStmt(forStmt));

		BaseAnnotation::AnnotationList annos;
		annos.push_back(std::make_shared<LoopAnnotation>(true));
		addAnnotations(forStmt, annos);
		EXPECT_TRUE(analysis::isIndependentStmt(forStmt));
	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
