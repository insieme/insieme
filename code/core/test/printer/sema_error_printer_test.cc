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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/attributes.h"

#include "insieme/core/annotations/error.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/core/lang/reference.h"

using namespace insieme::core;
using namespace insieme::core::printer;

// NOTE: this is deveolopement code, it does not really check anything, but is a
// nice example how to write a printer plugin, if it eventualy fails move it
// to own test
TEST(ErrorPrinter, address) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addrs = builder.parseAddressesStatement(R"1N5P1RE(
			decl lfun : (ref<int<4>>)->int<4>;
			def fun = (arg : int<4>)->int<4> { return arg + 1; };
			def rfun = (arg : ref<int<4>>)->int<4> { return *arg;};
            {
			$var ref<int<4>,f,f,plain> a;$
			var ref<int<4>,f,f,plain> b;
			var ref<int<4>,f,f,plain> c;
			var ref<int<4>,f,f,plain> d;
			var ref<int<4>,f,f,plain> e;
			var ref<int<4>,f,f,plain> f;
			var ref<int<4>,f,f,plain> g;
			{
				a = 7;
				fun(*b);
				rfun(c);
				fun(fun(*d));
				fun(rfun(e));
				$lfun$(f);
				rfun(ref_temp_init(lfun(g)));
			}
		}
	)1N5P1RE");
	EXPECT_EQ(2, addrs.size());

	auto stmt = addrs[0].getRootNode();

	struct AddressPlug : public PrinterPlugin {
		const NodePtr& root;
		const std::vector<NodeAddress>& addresses;
		mutable std::vector<std::string> nextLineMessages;

		AddressPlug(const NodePtr& root, const std::vector<NodeAddress>& addresses) : root(root), addresses(addresses) {
			// add annotations for every node in the error list
			visitDepthFirst(NodeAddress(root), [&](const NodeAddress& cur) {
				if(std::find(addresses.begin(), addresses.end(), cur) != addresses.end()) {
					annotations::attachError(cur.getAddressedNode(), "Addressed Node");
				}
			});
		}

		bool covers(const NodeAddress& n) const {
			if(annotations::hasAttachedError(n)) { return true; }

			return false;
		}

		std::ostream& print(std::ostream& out, const NodeAddress& n, const std::function<void(const NodeAddress&)>& f) const {
			static const std::string RED = "\033[31m";
			static const std::string RESET = "\033[0m";
			static const std::string GREY = "\033[37m";

			assert_true(annotations::hasAttachedError(n));

			// get and attach next line error
			auto error = annotations::getAttachedError(n);

			nextLineMessages.push_back(GREY + error + RESET);

			out << RED;
			out << PrettyPrinter(n);
			out << RESET;
			return out;
		}

		virtual std::ostream& afterNewLine(std::ostream& out) const {
			for(unsigned i = 0; i < nextLineMessages.size(); ++i) {
				out << format("%s\n", nextLineMessages[i]);
			}
			nextLineMessages.clear();
			return out;
		}
	};

	AddressPlug plug(stmt, addrs);

	PrettyPrinter p(stmt, plug);
	std::cout << p << std::endl;
}

TEST(ErrorPrinter, error) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto valueType = builder.getLangBasic().getInt4();
	auto var = builder.variable(valueType);
	auto expr = builder.literal("1", valueType);
	auto stmt = builder.callExpr(mgr.getLangBasic().getUnit(), mgr.getLangExtension<lang::ReferenceExtension>().getRefAssign(), var, expr);

	auto msgs = checks::check(stmt);

	std::stringstream ss;
	dumpErrors(msgs, ss);
	EXPECT_EQ("\x1B[33mv1 = 1\x1B[0m\n------- \n\x1B[31mERROR: \x1B[37mInvalid non-materializing argument: \n\t\tdecl ref<ref<'a,f,'v,'k>,f,f,plain> : v1\n\t\t - init expr of type int<4>\x1B[0m\n-------\n\n",
	          ss.str());

	std::cout << ss.str() << std::endl;
}
