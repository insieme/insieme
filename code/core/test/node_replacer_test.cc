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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"

#include "ir_dummy_annotations.inc"

#include "insieme/utils/logging.h"
#include "insieme/core/transform/manipulation_utils.h"

namespace insieme {
namespace core {

	using namespace checks;

	TEST(NodeReplacer, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a simple AST construct
		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");
		TypePtr typeC = builder.genericType("C", toVector(typeA, typeB, typeA));
		TypePtr typeD = builder.genericType("D");

		EXPECT_EQ("C<A,B,A>", toString(*typeC));

		NodePtr mod;

		mod = transform::replaceAll(manager, typeC, typeA, typeD);
		EXPECT_EQ("C<D,B,D>", toString(*mod));

		mod = transform::replaceAll(manager, typeC, typeB, typeD);
		EXPECT_EQ("C<A,D,A>", toString(*mod));

		mod = transform::replaceAll(manager, typeC, typeC, typeD);
		EXPECT_EQ("D", toString(*mod));
	}


	TEST(NodeReplacer, SkipperTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a simple AST construct
		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");
		TypePtr typeC = builder.genericType("C", toVector(typeA, typeB, typeA));
		TypePtr typeD = builder.genericType("D");

		EXPECT_EQ("C<A,B,A>", toString(*typeC));

		NodePtr mod;

		mod = transform::replaceAll(manager, typeC, typeA, typeD, [&](const NodePtr& node) {
			if(node == typeA) { return transform::ReplaceAction::Prune; }
			return transform::ReplaceAction::Process;
		});
		EXPECT_EQ("C<A,B,A>", toString(*mod));

		mod = transform::replaceAll(manager, typeC, typeA, typeD, [&](const NodePtr& node) {
			if(node == typeB) { return transform::ReplaceAction::Prune; }
			return transform::ReplaceAction::Process;
		});
		EXPECT_EQ("C<D,B,D>", toString(*mod));

		NodeMap nodemap;
		nodemap[typeA] = typeB;
		nodemap[typeB] = typeC;
		mod = transform::replaceAll(manager, typeC, nodemap, [&](const NodePtr& node) {
			if(node == typeA || node == typeB) { return transform::ReplaceAction::Prune; }
			return transform::ReplaceAction::Process;
		});
		EXPECT_EQ("C<A,B,A>", toString(*mod));

		mod = transform::replaceAll(manager, typeC, typeB, typeD, [&](const NodePtr& node) {
			if(node == typeA) { return transform::ReplaceAction::Prune; }
			return transform::ReplaceAction::Process;
		});
		EXPECT_EQ("C<A,D,A>", toString(*mod));
	}


	TEST(NodeReplacer, AnnotationPreservation) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a simple AST construct
		GenericTypePtr typeA = builder.genericType("A");
		GenericTypePtr typeB = builder.genericType("B");
		GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
		GenericTypePtr typeD = builder.genericType("D");

		typeD->addAnnotation(std::make_shared<DummyAnnotation>(12));
		typeC->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation>(16));
		typeC->addAnnotation(std::make_shared<DummyAnnotation>(10));

		EXPECT_EQ("C<A,B,A>", toString(*typeC));
		EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(typeD->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(typeC->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

		GenericTypePtr mod;

		mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeA, typeD));
		EXPECT_EQ("C<D,B,D>", toString(*mod));
		EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

		// check preservation of replacement annotations
		typeD->addAnnotation(std::make_shared<DummyAnnotation>(14));
		mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeB, typeD));
		EXPECT_EQ("C<A,D,A>", toString(*mod));
		EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(1)->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

		// ---- TEST preservation of annotations to modified nodes ----
		// add both annotations
		typeC->addAnnotation(std::make_shared<DummyAnnotation2>(20));
		EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation2::DummyKey));

		mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeB, typeD));
		EXPECT_EQ("C<A,D,A>", toString(*mod));

		// only one should have been preserved
		EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->hasAnnotation(DummyAnnotation2::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(1)->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
	}

	TEST(NodeReplacer, ReplaceByAddress) {
		NodeManager nm;
		IRBuilder builder(nm);

		// OK ... create a simple AST construct
		GenericTypePtr typeA = builder.genericType("A");
		GenericTypePtr typeB = builder.genericType("B");
		GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
		GenericTypePtr typeD = builder.genericType("D", toVector<TypePtr>(typeC));
		GenericTypePtr typeX = builder.genericType("X");

		EXPECT_EQ("D<C<A,B,A>>", toString(*typeD));

		typeD->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation>(12));
		typeD->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation2>(14));
		EXPECT_TRUE(typeD->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(typeD->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

		GenericTypeAddress addrD(typeD);
		GenericTypeAddress addrC = static_address_cast<GenericTypeAddress>(addrD->getTypeParameter(0));
		GenericTypeAddress addrA1 = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(0));
		GenericTypeAddress addrB = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(1));
		GenericTypeAddress addrA2 = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(2));

		GenericTypePtr mod;
		NodeManager manager;
		mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrA1, typeX));
		EXPECT_EQ("D<C<X,B,A>>", toString(*mod));
		EXPECT_FALSE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

		mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrA2, typeX));
		EXPECT_EQ("D<C<A,B,X>>", toString(*mod));
		EXPECT_FALSE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

		mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrD, typeX));
		EXPECT_EQ("X", toString(*mod));
	}

	TEST(NodeReplacer, ReplaceAllByAddress) {
		NodeManager nm;
		IRBuilder builder(nm);

		// OK ... create a simple AST construct
		GenericTypePtr typeA = builder.genericType("A");
		GenericTypePtr typeB = builder.genericType("B");
		GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
		GenericTypePtr typeD = builder.genericType("D", toVector<TypePtr>(typeC));

		GenericTypePtr typeX = builder.genericType("X");
		GenericTypePtr typeY = builder.genericType("Y");
		GenericTypePtr typeZ = builder.genericType("Z");

		EXPECT_EQ("D<C<A,B,A>>", toString(*typeD));

		GenericTypeAddress typeCAdr = static_address_cast<GenericTypeAddress>(GenericTypeAddress(typeD)->getTypeParameter(0));

		std::map<NodeAddress, NodePtr> replacements;
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
		EXPECT_EQ("D<C<X,B,A>>", toString(*transform::replaceAll(nm, replacements)));

		replacements.clear();
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(1), typeY));
		EXPECT_EQ("D<C<X,Y,A>>", toString(*transform::replaceAll(nm, replacements)));

		replacements.clear();
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(1), typeY));
		replacements.insert(std::make_pair(typeCAdr->getTypeParameter(2), typeZ));
		EXPECT_EQ("D<C<X,Y,Z>>", toString(*transform::replaceAll(nm, replacements)));
	}

	TEST(TransformBottomUp, Basic) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& basic = builder.getLangBasic();

		auto comp = builder.parseStmt(R"({
			var int<4> a = 0;
			var int<4> b = 1;
			if(true) {
				var int<4> c = a;
			}
		})");

		auto res = checks::check(comp);
		ASSERT_TRUE(res.empty()) << res;
		ASSERT_TRUE(analysis::contains(comp, basic.getInt4()));
		ASSERT_FALSE(analysis::contains(comp, basic.getReal4()));

		comp = transform::transformBottomUpGen(comp, [&](const TypePtr& typePtr) {
			if(basic.isInt4(typePtr)) {
				return basic.getReal4();
			}
			return typePtr;
		}, transform::globalReplacement);

		res = checks::check(comp);
		EXPECT_TRUE(res.empty()) << res;
		EXPECT_FALSE(analysis::contains(comp, basic.getInt4()));
		EXPECT_TRUE(analysis::contains(comp, basic.getReal4()));
	}

	TEST(TransformBottomUp, Limited) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& basic = builder.getLangBasic();

		auto comp = builder.parseAddressesStatement(R"(
		def fun = () -> unit {
			var int<4> c = 0;
			$c$;
		};
		{
			var int<4> a = 1;
			$a$;
			fun();
		})");
		ASSERT_TRUE(comp.size() == 2);

		auto root = comp[0].getRootNode();
		auto res = checks::check(root);
		ASSERT_TRUE(res.empty()) << res;
		ASSERT_EQ(basic.getInt4(), comp[0].as<ExpressionPtr>()->getType());
		ASSERT_EQ(basic.getInt4(), comp[1].as<ExpressionPtr>()->getType());

		root = transform::transformBottomUpGen(root, [&](const TypePtr& typePtr) {
			if(basic.isInt4(typePtr)) {
				return basic.getReal4();
			}
			return typePtr;
		}, transform::localReplacement);

		res = checks::check(root);

		EXPECT_TRUE(res.empty()) << res;
		EXPECT_EQ(basic.getReal4(), comp[0].switchRoot(root).as<ExpressionPtr>()->getType());
		EXPECT_EQ(basic.getInt4(), comp[1].switchRoot(root).as<ExpressionPtr>()->getType());
	}

	TEST(TransformBottomUp, Pruning) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& basic = builder.getLangBasic();

		auto fun = builder.parseExpr("lit(\"fun\" : ('t) -> unit)");


		auto comp = builder.parseAddressesStatement(R"(
		{
			var int<4> a = 0;
			$a$;
			fun($a$);
		})", IRBuilderBaseModule::EagerDefinitionMap { {"fun", fun} });
		ASSERT_TRUE(comp.size() == 2);

		auto root = comp[0].getRootNode();
		auto res = checks::check(root);
		ASSERT_TRUE(res.empty()) << res;
		ASSERT_EQ(basic.getInt4(), comp[0].as<ExpressionPtr>()->getType());
		ASSERT_EQ(basic.getInt4(), comp[1].as<ExpressionPtr>()->getType());

		root = transform::transformBottomUpGen(root, [&](const TypePtr& typePtr) {
			if(basic.isInt4(typePtr)) {
				return basic.getReal4();
			}
			return typePtr;
		}, [&fun](const core::NodePtr& cur) {
			//calls to our type_instantiation literal are pruned
			if(cur->getNodeType() == core::NT_CallExpr && core::analysis::isCallOf(cur, fun)) {
				return core::transform::ReplaceAction::Prune;
			}
			return core::transform::ReplaceAction::Process;
		});

		res = checks::check(root);

		EXPECT_TRUE(res.empty()) << res;
		EXPECT_EQ(basic.getReal4(), comp[0].switchRoot(root).as<ExpressionPtr>()->getType());
		EXPECT_EQ(basic.getInt4(), comp[1].switchRoot(root).as<ExpressionPtr>()->getType());
	}

	TEST(TransformBottomUp, Nested) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto comp = builder.parseStmt(R"({
			{
				{ }
				5;
				{ 6; { } }
				{ { {} } }
				{ { { 5; { } } } }
				{ }
			}
		})");

		auto res = checks::check(comp);
		ASSERT_TRUE(res.empty()) << res;
		ASSERT_TRUE(analysis::contains(comp, builder.compoundStmt()));

		comp = transform::transformBottomUpGen(comp, [&](const CompoundStmtPtr& compound) {
			StatementList statements;
			for(auto stmt: compound) {
				auto compStmt = stmt.isa<CompoundStmtPtr>();
				if(!compStmt || compStmt.size() > 0) statements.push_back(stmt);
			}
			return builder.compoundStmt(statements);
		}, transform::globalReplacement);

		res = checks::check(comp);
		EXPECT_TRUE(res.empty()) << res;
		EXPECT_FALSE(analysis::contains(comp, builder.compoundStmt()));
	}

} // end namespace core
} // end namespace insieme
