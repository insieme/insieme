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
#include "insieme/core/analysis/ir_utils.h"

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

	TEST(isReadOnly, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto typeA = core::lang::buildRefType(mgr.getLangBasic().getInt4(), true, false, core::lang::ReferenceType::Kind::Plain);
		auto typeB = core::lang::buildRefType(mgr.getLangBasic().getInt4(), false, false, core::lang::ReferenceType::Kind::Plain);

		auto declA = builder.declarationStmt(typeA, builder.intLit(0));
		auto declB = builder.declarationStmt(typeB, builder.intLit(0));
		auto declC = builder.declarationStmt(typeB, builder.intLit(0));

		core::StatementList stmts;
		stmts.push_back(declA);
		stmts.push_back(declB);
		stmts.push_back(declC);
		auto comp = builder.compoundStmt(builder.assign(declB->getVariable(), builder.intLit(1)));
		stmts.push_back(comp);
		auto stmt = builder.compoundStmt(stmts);

		EXPECT_TRUE(opencl::analysis::isReadOnly(comp, declA->getVariable()));
		EXPECT_FALSE(opencl::analysis::isReadOnly(comp, declB->getVariable()));
		EXPECT_TRUE(opencl::analysis::isReadOnly(comp, declC->getVariable()));
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

	TEST(DISABLED_getDependencyGraph, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		core::IRBuilder::EagerDefinitionMap symbols;
		symbols["fun0"] = builder.parseExpr("(lhs : ref<'a,f,'b>, rhs : 'a) -> 'a { lhs = rhs; return *lhs; }");

		auto stmt = builder.parseStmt("{ var ref<int<4>,f,f> v1 = ref_temp_init(1); fun0(v1, *v1+1); }", symbols);

		analysis::DependencyGraph graph;
		std::cout << dumpColor(stmt);

		auto vars = core::analysis::getAllVariablesInScope(stmt);
		analysis::getDependencyGraph(stmt, vars, graph);

		for (const core::VariablePtr& var : vars) {
			std::cout << "tree starting at root: " << dumpColor(var);
			analysis::visitDepthFirstOnce(graph, [&](const core::NodePtr& vertex) {
				std::cout << "vertex: " << dumpColor(vertex);
				if (vertex->getNodeType() == core::NT_CallExpr) {
					// yeah expand here
					analysis::DependencyGraph child;
					core::NodeMap mapping;

					analysis::getDependencyGraph(vertex.as<core::CallExprPtr>(), mapping, child);
					for (const auto& pair : mapping) {
						std::cout << dumpColor(pair.first) << " --> " << dumpColor(pair.second);
					}
				}
			});
		}
	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
