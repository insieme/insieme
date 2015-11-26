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

#include <string>
#include <fstream>
#include <functional>

#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/regex.hpp>

#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/ir_address.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/utils/config.h"

namespace insieme {
namespace frontend {

	using namespace extensions;
	using namespace core;
	using insieme::annotations::ExpectedIRAnnotation;

	namespace {

		void irDiff(NodePtr a, NodePtr b) {
			if(a->getNodeType() != b->getNodeType()) {
				std::cout << "IRDIFF-----\nNode types differ:\n" 
					<< "NT a: " << a->getNodeType() << "\n" << "NT b: " << b->getNodeType() << "\n"
					<< "a: " << *a << "\nb: " << *b << "\n";
			}
			
			auto aExp = a.isa<ExpressionPtr>();
			auto bExp = b.isa<ExpressionPtr>();
			if(aExp) {
				if(aExp->getType() != bExp->getType()) {
					std::cout << "IRDIFF-----\nExpressions differ in type:\n" 
						<< "type a: " << *aExp->getType() << "\n" << "type b: " << *bExp->getType() << "\n"
						<< "a: " << *a << "\nb: " << *b << "\n";
				}
			}

			auto aVar = a.isa<VariablePtr>();
			auto bVar = b.isa<VariablePtr>();
			if(aVar) {
				if(aVar->getId() != bVar->getId()) {
					std::cout << "IRDIFF-----\nVariables differ in id:\n" 
						<< "id a: " << aVar->getId() << "\n" << "id b: " << bVar->getId() << "\n"
						<< "a: " << *a << "\nb: " << *b << "\n";
				}
			}
			
			auto aChildren = a->getChildList();
			auto bChildren = b->getChildList();
			if(aChildren.size() != bChildren.size()) {				
					std::cout << "IRDIFF-----\nNodes differ in number of children (ABORTING):\n" 
						<< "child count a: " << aChildren.size() << "\n" << "child count b: " << bChildren.size() << "\n"
						<< "a: " << *a << "\nb: " << *b << "\n";
					return;
			}

			for(size_t i=0; i<aChildren.size(); ++i) {
				irDiff(aChildren[i], bChildren[i]);
			}
		}

		
		static inline std::string locationOf(const NodeAddress& addr) {
			auto loc = core::annotations::getLocation(addr);
			return loc ? toString(*loc) : std::string("-");
		}

		static inline void checkExpected(NodePtr expected, NodePtr actual, const NodeAddress& addr) {
			ASSERT_TRUE(actual) << "Actual IR pointer null!";
			ASSERT_TRUE(expected) << "Expected IR pointer null (likely parser error)!\n" << "  Location: " << *core::annotations::getLocation(addr) << "\n";
			IRBuilder builder(expected->getNodeManager());
			bool eIsExp = expected.isa<ExpressionPtr>();
			bool aIsExp = actual.isa<ExpressionPtr>();
			auto expNN = expected;
			auto actNN = actual;
			expected = builder.normalize(expected);
			actual = builder.normalize(actual);
			EXPECT_EQ(expected, actual) << "Location     : " << locationOf(addr) << "\n"
			                            << "Actual Pretty: " << dumpColor(actual, std::cout, true) << "\n"
			                            << "Expect Pretty: " << dumpColor(expected, std::cout, true) << "\n"
			                            //<< "Actual NN: " << dumpColor(actNN, std::cout, true) << "\n"
			                            //<< "Expect NN: " << dumpColor(expNN, std::cout, true) << "\n"
			                            << "Actual type  : " << (aIsExp ? toString(dumpColor(actual.as<ExpressionPtr>()->getType())) : toString("-")) << "\n"
			                            << "Expected type: " << (eIsExp ? toString(dumpColor(expected.as<ExpressionPtr>()->getType())) : toString("-")) << "\n"
			                            //<< "Text actual  :\n" << dumpText(actual) << "\n"
			                            //<< "Text expected:\n" << dumpText(expected) << "\n"
										;
			if(expected != actual) irDiff(actual, expected);
		}
	}

	static inline void runIndependentTestOn(const string& fn, std::function<void(ConversionJob&)> jobModifier = [](ConversionJob& job) {}) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		ConversionJob job(fn);
		if(job.isCxx()) job.setStandard(ConversionSetup::Cxx11);
		job.registerFrontendExtension<TestPragmaExtension>([](conversion::Converter& converter, int num) {
			EXPECT_EQ(num, converter.getVarMan()->numVisibleDeclarations()) << "Location: " << converter.getLastTrackableLocation() << "\n";
		});
		job.registerFrontendExtension<FrontendCleanupExtension>();
		jobModifier(job);
		auto res = builder.normalize(job.execute(mgr));

		auto symbols = mgr.getLangExtension<frontend::utils::FrontendInspireModule>().getSymbols();

		// iterate over res and check pragma expectations
		size_t visited = 0;
		visitDepthFirstOnce(NodeAddress(res), [&](const NodeAddress& addr) {
			auto node = addr.getAddressedNode();
			if(node->hasAnnotation(ExpectedIRAnnotation::KEY)) {
				auto ann = node->getAnnotation(ExpectedIRAnnotation::KEY);
				auto ex = ann->getExpected();
				
				const string regexKey = "REGEX";
				const string regexKeyS = "REGEX_S";
				const string stringKey = "STRING";
				const string exprTypeKey = "EXPR_TYPE";

				VLOG(2) << "Expected annotation string: " << ex << "\n";

				// Regex expect
				if(boost::starts_with(ex, regexKey)) {
					boost::regex re;
					if(boost::starts_with(ex, regexKeyS)) {
						auto res = ex.substr(regexKeyS.size());
						boost::replace_all(res, " ", "\\s*");
						re = boost::regex(res);
					} else {
						re = boost::regex(ex.substr(regexKey.size()));
					}
					auto irString = ::toString(printer::PrettyPrinter(
					    builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT | printer::PrettyPrinter::PRINT_DERIVED_IMPL | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_TRUE(boost::regex_match(irString.begin(), irString.end(), re)) << "Location : " << locationOf(addr) << "\n"
					                                                                      << "IR String: " << irString << "\n"
					                                                                      << "Regex    : " << re << "\n";
				} else if(boost::starts_with(ex, stringKey)) {
					string exs(ex.substr(stringKey.size()));
					auto irString = ::toString(printer::PrettyPrinter(
					    builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT | printer::PrettyPrinter::NO_LET_BINDINGS
					                                 | printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_EQ(exs, irString) << "Location       : " << locationOf(addr) << "\n";
				} else if(boost::starts_with(ex, exprTypeKey)) {
					string irs(ex.substr(exprTypeKey.size()));
					NodePtr expected = builder.parseType(irs, symbols);
					checkExpected(expected, node.as<ExpressionPtr>()->getType(), addr);
				} else {
					NodePtr expected;
					if(node.isa<ExpressionPtr>()) {
						expected = builder.parseExpr(ex, symbols);
					} else {
						expected = builder.parseStmt(ex, symbols);
					}
					checkExpected(expected, node, addr);
				}
				visited++;
			}
		});

		std::ifstream tf(fn);
		std::string fileText((std::istreambuf_iterator<char>(tf)), std::istreambuf_iterator<char>());
		std::string searchString{"#pragma test expect_ir"};
		string::size_type start = 0;
		unsigned occurrences = 0;
		while((start = fileText.find(searchString, start)) != string::npos) { 
			++occurrences; 
			start += searchString.length();
		}
		EXPECT_EQ(visited, occurrences);

		//dumpColor(res);
		//std::cout << res;

		//visitDepthFirstOnce(res, [](const LiteralPtr& lit) { 
		//	std::cout << "Literal: " << *lit << "\n of type " << *lit->getType() << "\n\n";
		//});

		auto checkResult = core::checks::check(res);
		EXPECT_EQ(checkResult.size(), 0) << checkResult;
		//std::cout << "Semantic Error dump:\n";
		//dumpText(checkResult.getErrors()[0].getOrigin().getParentNode(2));
	}
} // end namespace frontend
} // end namespace insieme
