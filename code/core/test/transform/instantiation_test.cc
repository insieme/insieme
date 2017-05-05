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

#include "insieme/core/transform/instantiate.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace core {
namespace transform {

	TEST(TypeVariableInstantiation, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def x = (a : 'a)->unit {
				auto z = a;
				$z$;
			};
			def test = (f : 'a) -> unit {
				auto y = f;
				x(f);
			};
			{
				var int<4> x = 1;
				test(x);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
	}

	TEST(TypeVariableInstantiation, ExpressionArgument) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def x = (a : 'a)->unit {
				auto z = a;
				$z$;
			};
			def test = (f : 'a) -> unit {
				auto y = f;
				x(f);
			};
			{
				var int<4> x = 1;
				test(x*5);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
	}

	TEST(TypeInstantiation, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def test = (a : vector<'res,'l>) -> unit {
				$var ref<vector<'res, 'l>> res;$
			};
			{
				var ref<vector<int<4>, 8>> a;
				test(*a);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);

		EXPECT_EQ(builder.normalize(builder.parseStmt("var ref<vector<int<4>, 8>> res;")), builder.normalize(newAddr.getAddressedNode()));
	}

	TEST(TypeInstantiation, BuiltinPreserving) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def bla = (a : 'a) -> 'a { return a; };
			def test = (a : vector<'res,'l>) -> unit {
				bla($ref_temp(type_lit(vector<'res,'l>))$);
			};
			{
				var ref<vector<int<4>, 8>> a;
				test(*a);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);

		EXPECT_EQ(builder.normalize(builder.parseExpr("ref_temp(type_lit(vector<int<4>, 8>))")), builder.normalize(newAddr.getAddressedNode()));
		EXPECT_TRUE(core::lang::isBuiltIn(newAddr.as<CallExprPtr>()->getFunctionExpr()));
	}

	TEST(TypeInstantiation, Constructor) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			{
				var ref<IMP_TemplateWithMethod<int<4>>,f,f,plain> v0 = $lit("IMP_TemplateWithMethod::ctor" : IMP_TemplateWithMethod<'T>::())$(v0);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);

		EXPECT_EQ(builder.parseType("IMP_TemplateWithMethod<'T>::()"), addresses[0].getAddressedNode().as<LiteralPtr>()->getType());
		EXPECT_EQ(builder.parseType("IMP_TemplateWithMethod<int<4>>::()"), newAddr.getAddressedNode().as<LiteralPtr>()->getType());
	}

	TEST(TypeInstantiation, NameAnnotations) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def test = (a : vector<'res,'l>) -> unit {
				$var ref<vector<'res, 'l>> res;$
				var int<4> x = 0;
				$x$;
				$res$;
			};
			{
				var ref<vector<int<4>, 8>> a;
				test(*a);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 3);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		auto newAnnAddr = addresses[1].switchRoot(result);
		auto newAnnAddr2 = addresses[2].switchRoot(result);
		EXPECT_EQ(builder.normalize(builder.parseStmt("var ref<vector<int<4>, 8>> res;")), builder.normalize(newAddr.getAddressedNode()));
		EXPECT_TRUE(annotations::hasAttachedName(addresses[1].getAddressedNode()));
		EXPECT_TRUE(annotations::hasAttachedName(newAnnAddr.getAddressedNode()));
		EXPECT_TRUE(annotations::hasAttachedName(newAnnAddr2.getAddressedNode()));
		EXPECT_EQ(annotations::getAttachedName(addresses[1].getAddressedNode()), annotations::getAttachedName(newAnnAddr.getAddressedNode()));
		EXPECT_EQ(annotations::getAttachedName(addresses[2].getAddressedNode()), annotations::getAttachedName(newAnnAddr2.getAddressedNode()));
	}

	TEST(TypeInstantiation, TypeAnnotations) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
		def test = (a : vector<'res,'l>) -> unit {
			$var ref<vector<'res, 'l>> res;$
		};
		{
			var ref<vector<int<4>, 8>> a;
			test(*a);
		}
	)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto declStmtType = addresses[0].getAddressedNode().as<DeclarationStmtPtr>()->getVariable()->getType();
		annotations::attachName(declStmtType, "NewtypeGundam");

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		auto newDeclStmtType = newAddr.getAddressedNode().as<DeclarationStmtPtr>()->getVariable()->getType();
		EXPECT_TRUE(annotations::hasAttachedName(newDeclStmtType));
		EXPECT_EQ(annotations::getAttachedName(declStmtType), annotations::getAttachedName(newDeclStmtType));
		EXPECT_EQ("NewtypeGundam", annotations::getAttachedName(newDeclStmtType));
	}

	TEST(TypeInstantiation, AnnotationsOnCallExp) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
		def test = (a : vector<'res,'l>) -> unit {
			var ref<vector<'res, 'l>> res;
		};

		{
			var ref<vector<int<4>, 8>> a;
			$test(*a)$;
		}
	)raw");

		EXPECT_EQ(addresses.size(), 1);

		// attach a dummy annotation to the callExpr and check whether it is still there after the types have been instantiated
		auto root = addresses[0].getRootNode();
		annotations::attachLocation(addresses[0].getAddressedNode(), "dummy", 0, 1, 2, 3);
		EXPECT_TRUE(annotations::hasAttachedLocation(addresses[0].getAddressedNode()));

		auto result = instantiateTypes(root);
		auto newAddr = addresses[0].switchRoot(result);
		EXPECT_TRUE(annotations::hasAttachedLocation(newAddr.getAddressedNode()));
		EXPECT_EQ(annotations::getLocation(addresses[0].getAddressedNode()), annotations::getLocation(newAddr.getAddressedNode()));
	}

	TEST(TypeInstantiation, DISABLED_HigherOrderFunction) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def foo = (v : 'a) -> 'a {
				return $v$;
			};

			def test = (v : array<'res,'l>, f : ('res) -> 'res) -> unit {
				f(v[0]);
			};

			{
				var ref<array<int<4>, 8>> a;
				test(*a, foo);
			}
		)raw");

		EXPECT_EQ(addresses.size(), 1);

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>().getType());
	}

	TEST(TypeInstantiation, ReturnTypeSimple) {
		NodeManager mgr;
		IRBuilder build(mgr);

		auto expr = build.parseExpr(R"(
		(arg : 'a) -> 'a {
				return arg;
		}(5)
		)");

		EXPECT_TRUE(expr);

		auto instantiated = core::transform::instantiateTypes(expr);

		// std::cout << "Pretty uninstantiated: \n" << dumpColor(expr) << "\n";
		// std::cout << "Pretty instantiation : \n" << dumpColor(instantiated) << "\n";
		EXPECT_EQ(build.parseType("int<4>"), instantiated->getType());
	}

	TEST(TypeInstantiation, BindExpr) {
		NodeManager mgr;
		IRBuilder build(mgr);

		auto expr = build.parseExpr(R"(
			(x : '_type_) -> unit {
				var ref<'_type_> b;
				() => {
					auto y = b;
					return y;
				};
			}(5)
		)");

		EXPECT_TRUE(expr);

		auto instantiated = core::transform::instantiateTypes(expr);

		// std::cout << "Pretty uninstantiated: \n" << dumpColor(expr) << "\n";
		// std::cout << "Pretty instantiation : \n" << dumpColor(instantiated) << "\n";
		// std::cout << "Less pretty instantiation: \n" << dumpText(instantiated) << "\n";
		EXPECT_TRUE(core::analysis::contains(expr, build.parseType("'_type_")));
		EXPECT_FALSE(core::analysis::contains(instantiated, build.parseType("'_type_")));
	}

	TEST(TypeInstantiation, BindInBindExpr) {
		NodeManager mgr;
		IRBuilder build(mgr);

		auto expr = build.parseExpr(R"(
			(x : '_type_) -> unit {
				var ref<'_type_> b;
				() => {
					auto y = b;
					() => {
						auto z = y;
						return z;
					};
					return y;
				};
			}(5)
		)");

		EXPECT_TRUE(expr);

		auto instantiated = core::transform::instantiateTypes(expr);

		// std::cout << "Pretty uninstantiated: \n" << dumpColor(expr) << "\n";
		// std::cout << "Pretty instantiation : \n" << dumpColor(instantiated) << "\n";
		EXPECT_TRUE(core::analysis::contains(expr, build.parseType("'_type_")));
		EXPECT_FALSE(core::analysis::contains(instantiated, build.parseType("'_type_")));
	}

	TEST(TypeInstantiation, JobExpr) {
		NodeManager mgr;
		IRBuilder build(mgr);

		auto expr = build.parseExpr(R"(
			(x : '_type_) -> unit {
				var ref<'_type_> b;
				parallel(job {
					auto y = b;
				});
			}(5)
		)");

		EXPECT_TRUE(expr);

		auto instantiated = core::transform::instantiateTypes(expr);

		// std::cout << "Pretty uninstantiated: \n" << dumpColor(expr) << "\n";
		// std::cout << "Pretty instantiation : \n" << dumpColor(instantiated) << "\n";
		EXPECT_TRUE(core::analysis::contains(expr, build.parseType("'_type_")));
		EXPECT_FALSE(core::analysis::contains(instantiated, build.parseType("'_type_")));
	}

	TEST(TypeInstantiation, NestedLambda) {
		NodeManager mgr;
		IRBuilder build(mgr);

		core::ProgramPtr program = build.parseProgram(
		    R"(
            alias int = int<4>;
            alias uint = uint<4>;

            def differentbla = (x : '_type_b) -> unit {
                auto m = x;
                auto l = m;
            };

            def anotherbla = (x : '_type_a) -> unit {
                auto m = x;
            };

            def bla = (f : '_type_a) -> unit {
                anotherbla(f);
                differentbla(f);
                parallel(job { auto l = f; });
            };

            int main() {
                // some bla
                var int x = 10;
                bla(x);
                return 0;
            }
            )");


		EXPECT_TRUE(program);

		auto instantiated = core::transform::instantiateTypes(program);

		// std::cout << "Pretty uninstantiated: \n" << dumpColor(program) << "\n";
		// std::cout << "Pretty instantiation : \n" << dumpColor(instantiated) << "\n";
		EXPECT_TRUE(core::analysis::contains(program, build.parseType("'_type_a")));
		EXPECT_TRUE(core::analysis::contains(program, build.parseType("'_type_b")));
		EXPECT_FALSE(core::analysis::contains(instantiated, build.parseType("'_type_a")));
		EXPECT_FALSE(core::analysis::contains(instantiated, build.parseType("'_type_b")));
	}


	TEST(Recursion, Simple) {
		NodeManager mgr;
		IRBuilder build(mgr);

		auto addresses = build.parseAddressesStatement(R"raw(
			decl x : ('_type_)->unit;
			def x = (a : '_type_)->unit {
				$x$(a); // address 1
			};
			{
				$x$(5); // address 0
			}
		)raw");

		EXPECT_EQ(addresses.size(), 2);

		auto code = addresses[0].getRootNode();
		auto result = instantiateTypes(code);

		EXPECT_TRUE(core::analysis::contains(code, build.parseType("'_type_")));
		EXPECT_FALSE(core::analysis::contains(result, build.parseType("'_type_")));

		auto add0res = addresses[0].switchRoot(result).getAddressedNode().as<LambdaExprPtr>();
		auto add1res = addresses[1].switchRoot(result);
		EXPECT_EQ(add0res->getReference(), add1res);
		EXPECT_EQ(build.parseType("(int<4>)->unit"), add0res->getReference()->getType());
	}

	TEST(InRecFunc, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
			def x = (a : 'a)->unit {
				auto z = a;
				$z$;
			};
			def test = (f : 'a) -> unit {
				auto y = f;
				x(f);
			};
			{
				var int<4> v = 1;
				test(v);
			}
		)raw");

		auto result = instantiateTypes(addresses[0].getRootNode());

		auto newAddr = addresses[0].switchRoot(result);
		EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
	}

	TEST(TypeInstantiation, ReturnTypeBug) {

		/**
		 * This code fragment has been encountered as being buggy in the backend.
		 * The return type of the utilized lambda has not been properly instantiated.
		 */

		NodeManager mgr;
		IRBuilder builder(mgr);

		core::ProgramPtr code = builder.parseProgram("int<4> main() {"
		                                             "  (dtype : type<'a>, size : type<'s>)->unit {"
		                                             "    var ref<'a> v0;"
		                                             "    var ref<int<'s>> v1;"
		                                             "  } (type_lit(real<4>), type_lit(8));"
		                                             "  return 0;"
		                                             "}");

		ASSERT_TRUE(code);

		EXPECT_TRUE(checks::check(code).empty()) << checks::check(code);

		auto ret = core::transform::instantiateTypes(code, core::lang::isBuiltIn);
		EXPECT_TRUE(checks::check(ret).empty()) << printer::dumpErrors(checks::check(ret));
	}


	TEST(TypeInstantiation, ArrayPointwiseTest) {

		/**
		 * This code fragment has been encountered as being buggy in the backend.
		 * The return type of the utilized lambda has not been properly instantiated.
		 */

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseExpr("array_pointwise(int_add)");
		ASSERT_TRUE(code);
		EXPECT_TRUE(checks::check(code).empty()) << checks::check(code);
		auto ret = core::transform::instantiateTypes(code, core::lang::isBuiltIn);
		EXPECT_TRUE(checks::check(ret).empty()) << printer::dumpErrors(checks::check(ret));

		code = builder.parseExpr("array_pointwise(int_add)(lit(\"x\":array<int<4>,12u>),lit(\"y\":array<int<4>,12u>))");
		ASSERT_TRUE(code);
		EXPECT_TRUE(checks::check(code).empty()) << checks::check(code);
		ret = core::transform::instantiateTypes(code, core::lang::isBuiltIn);
		EXPECT_TRUE(checks::check(ret).empty()) << printer::dumpErrors(checks::check(ret));
	}

	// ********** The following tests should be enabled / completed once instantiation
	// ********** of mutually recursive functions is supported

	/*
	TEST(InRecFunc, ExpressionArgument) {
	    NodeManager mgr;
	    IRBuilder builder(mgr);

	    auto addresses = builder.parseAddressesStatement(R"raw(
	    {
	        let x, test= lambda ('a a)->unit {
	            decl auto z = a;
	            $z$;
	        },
	        lambda ('a f) -> unit {
	            decl auto y = f;
	            x(f);
	        };
	        decl int<4> v = 1;
	        test(v*5);
	    }
	    )raw");

	    EXPECT_EQ(addresses.size(), 1);

	    auto result = instantiateTypes(addresses[0].getRootNode());

	    auto newAddr = addresses[0].switchRoot(result);
	    EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
	}
	*/

} // end namespace transform
} // end namespace core
} // end namespace insieme
