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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

	TEST(BuilderTest, CreateCallExprFromBody) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		const lang::BasicGenerator& gen = mgr.getLangBasic();

		DeclarationStmtPtr a = builder.declarationStmt(builder.variable(gen.getInt4()));

		StatementPtr stmt = builder.parseStmt(R"(
		{
			let fun = lambda (int<4> arg)->int<4> { return arg + 1; };
			let lfun = expr lit("lfun":(ref<int<4>>)->int<4>);
			let rfun = lambda (ref<int<4>> arg)->int<4> { return *arg;};
		
			decl ref<int<4>> a;
			decl ref<int<4>> b;
			decl ref<int<4>> c;
			decl ref<int<4>> d;
			decl ref<int<4>> e;
			decl ref<int<4>> f;
			decl ref<int<4>> g;
			{
				a = 7;
				fun(*b);
				rfun(c);
				fun(fun(*d));
				fun(rfun(e));
				lfun(f);
				rfun(var(lfun(g)));
			}
		}
        )");

		//	std::cout << " ***************************** " << std::endl;
		//	dumpPretty(stmt);
		//	std::cout << " ***************************** " << std::endl;

		StatementPtr body;
		visitDepthFirstPrunable(stmt, [&](const CompoundStmtPtr& cs) {
			if(cs->getChildList().size() == 7) {
				body = cs;
				return true;
			}
			return false;
		});

		EXPECT_TRUE(body);

		NodePtr call = builder.createCallExprFromBody(body, gen.getUnit());
		NodePtr embeddedCall = transform::replaceAll(mgr, stmt, body, call);

		//	dumpPretty(embeddedCall);
		//	std::cout << " ***************************** " << std::endl;

		auto semantic = core::checks::check(embeddedCall);
		auto warnings = semantic.getWarnings();
		std::sort(warnings.begin(), warnings.end());
		for_each(warnings, [](const core::checks::Message& cur) { LOG(INFO) << cur << std::endl; });

		auto errors = semantic.getErrors();
		EXPECT_EQ(0u, errors.size());

		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::checks::Message& cur) { std::cout << cur << std::endl; });
	}


	TEST(BuilderTest, CreateCallExprFromBody1) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		const lang::BasicGenerator& gen = mgr.getLangBasic();

		std::map<string, core::NodePtr> symbols;

		core::VariablePtr varA = builder.variable(builder.refType(gen.getInt4()));
		core::VariablePtr varB = builder.variable(builder.refType(gen.getInt4()));

		symbols["var1"] = varA;
		symbols["var2"] = varB;
		symbols["var3"] = builder.variable(builder.refType(gen.getInt4()));
		symbols["var4"] = builder.variable(builder.refType(gen.getInt4()));

		DeclarationStmtPtr declA = builder.declarationStmt(varA, builder.getZero(builder.refType(gen.getInt4())));
		DeclarationStmtPtr declB = builder.declarationStmt(varB, builder.getZero(builder.refType(gen.getInt4())));

		core::StatementPtr assignStmt1 = builder.parseStmt("var1 = *var3;", symbols);
		core::StatementPtr assignStmt2 = builder.parseStmt("var4 = *var2;", symbols);

		// body with 2 decl stmts and usage of 4 variables
		core::StatementList bodyStmts;
		bodyStmts.push_back(declA);
		bodyStmts.push_back(declB);
		bodyStmts.push_back(assignStmt1);
		bodyStmts.push_back(assignStmt2);

		core::CompoundStmtPtr body = builder.compoundStmt(bodyStmts);
		core::CallExprPtr call = builder.createCallExprFromBody(body, gen.getUnit()).as<core::CallExprPtr>();

		EXPECT_EQ(2u, call->getArguments().size());
	}


	struct BuilderModule {
		NodeManager& mgr;
		BuilderModule(NodeManager& mgr) : mgr(mgr) {}
	};

	template<typename ... Modules>
	struct Builder : public virtual Modules... {
		Builder(NodeManager& mgr) : Modules(mgr)...  {}

		int f() { return 0; };
	};

	struct DefaultBuilder : public Builder<> {
		DefaultBuilder(NodeManager& mgr) : Builder<>(mgr) {}
	};

	struct ModuleA : public BuilderModule {
		ModuleA(NodeManager& mgr) : BuilderModule(mgr) {}
		int a() { return 0; };
	};

	struct ModuleB : public BuilderModule {
		ModuleB(NodeManager& mgr) : BuilderModule(mgr) {}
		int b() { return 0; };
	};


	TEST(BuilderTest, ModularStructure) {

		NodeManager mgr;

		DefaultBuilder builder(mgr);
		builder.f();

		Builder<ModuleA> builderA(mgr);
		builderA.a();

		Builder<ModuleA, ModuleB> builderAB(mgr);
		builderAB.a();
		builderAB.b();


	}


}
}
