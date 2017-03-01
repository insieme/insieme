/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <gtest/gtest.h>

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/opencl/opencl_analysis.h"
#include "insieme/backend/opencl/opencl_transform.h"

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

	TEST(isVariableRequirementOf, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto code = builder.parseStmt(R"(
			{
				var ref<int<4>,f,f> x = ref_temp_init(0);
				for(int<4> i = 0 .. 10 : 1) { var ref<int<4>> y = ref_temp_init(*x); }
			}
		)");

		core::ForStmtPtr forStmt;
		core::visitDepthFirstOncePrunable(code, [&](const core::ForStmtPtr& stmt) {
			forStmt = stmt;
			return true;
		});
		EXPECT_TRUE(!!forStmt);

		auto requirements = analysis::getVariableRequirements(mgr, code, forStmt);
		EXPECT_GT(requirements.size(), 0);

		auto callExpr = transform::outline(mgr, forStmt, requirements);
		EXPECT_EQ(callExpr->getArgumentList().size(), 1);

		auto lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		EXPECT_TRUE(analysis::isVariableRequirementOf(lambdaExpr, requirements));
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
