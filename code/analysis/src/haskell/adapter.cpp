/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/haskell/adapter.h"

#include "insieme/analysis/common/failure.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/container_utils.h"

#include <map>
#include <sstream>
#include <vector>

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary::haskell;

extern "C" {

	using insieme::analysis::ArithmeticSet;
	using insieme::analysis::haskell::Context;
	using insieme::analysis::haskell::StablePtr;

	// Haskell Runtime
	void hs_init(int, char*[]);
	void hs_exit(void);

	// Haskell Object Management
	void hat_freeStablePtr(StablePtr ptr);

	// Haskell Context
	StablePtr hat_initialize_context(const Context* ctx_c, const char* dump_c, size_t size_c);

	// NodePath
	StablePtr hat_mk_node_address(StablePtr ctx_hs, const size_t* path_c, size_t length_c);
	size_t hat_node_path_length(StablePtr addr_hs);
	void hat_node_path_poke(StablePtr addr_hs, size_t* path_c);

	// Analysis
	StablePtr hat_find_declaration(const StablePtr var_hs);
	int hat_check_boolean(const StablePtr expr_hs);
	int hat_check_alias(const StablePtr x_hs, const StablePtr y_hs);
	ArithmeticSet* hat_arithmetic_value(Context* ctx_c, const StablePtr expr_hs);

}

namespace insieme {
namespace analysis {
namespace haskell {

	// ------------------------------------------------------------ Context
	Context::Context(NodePtr root) : root(root) {
		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format (includes builtins)
		dumpIR(buffer, root);

		// get data as C string
		const string dump = buffer.str();
		const char* dump_c = dump.c_str();

		// pass to Haskell
		context_hs = hat_initialize_context(this, dump_c, dump.size());
		assert_true(context_hs) << "could not initialize Haskell context";
	}

	Context::~Context() {
		for(auto pair : addresses) {
			hat_freeStablePtr(pair.second);
		}
		hat_freeStablePtr(context_hs);
	}

	Context::Context(Context&& other) : context_hs(other.context_hs), root(other.root) {}

	Context& Context::operator=(Context&& other) {
		root = other.root;
		context_hs = other.context_hs;
		return *this;
	}

	NodePtr Context::getRoot() const { return root; }

	VariableAddress Context::getDefinitionPoint(const VariableAddress& var) {
		StablePtr var_hs = addNodeAddress(var);
		StablePtr def = hat_find_declaration(var_hs);
		if(!def) return {};
		return getNodeAddress(def).as<VariableAddress>();
	}

	BooleanAnalysisResult Context::checkBoolean(const ExpressionAddress& expr) {
		StablePtr expr_hs = addNodeAddress(expr);
		auto res = static_cast<BooleanAnalysisResult>(hat_check_boolean(expr_hs));
		if(res == BooleanAnalysisResult_Neither) {
			std::vector<std::string> msgs{"Boolean Analysis Error"};
			throw AnalysisFailure(msgs);
		}
		return res;
	}

	AliasAnalysisResult Context::checkAlias(const ExpressionAddress& x, const ExpressionAddress& y) {
		StablePtr x_hs = addNodeAddress(x);
		StablePtr y_hs = addNodeAddress(y);
		return static_cast<AliasAnalysisResult>(hat_check_alias(x_hs, y_hs));
	}

	ArithmeticSet Context::arithmeticValue(const ExpressionAddress& expr) {
		StablePtr expr_hs = addNodeAddress(expr);
		ArithmeticSet* res_ptr = hat_arithmetic_value(this, expr_hs);
		ArithmeticSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

	StablePtr Context::addNodeAddress(const NodeAddress& addr) {
		assert_eq(root, addr.getRootNode()) << "root node does not match this context";

		// check if address has already been moved to Haskell
		if(containsKey(addresses, addr)) return addresses[addr];

		// empty address corresponds to root node in Haskell
		size_t length_c = addr.getDepth() - 1;
		vector<size_t> path_c(length_c);

		for(size_t i = 0; i < length_c; i++) {
			path_c[i] = addr.getParentAddress(length_c - 1 - i).getIndex();
		}

		StablePtr addr_hs = hat_mk_node_address(context_hs, path_c.data(), length_c);
		assert_true(addr_hs) << "could not pass NodePath to Haskell";

		addresses[addr] = addr_hs;
		return addr_hs;
	}

	NodeAddress Context::getNodeAddress(StablePtr addr_hs) const {
		vector<size_t> dst(hat_node_path_length(addr_hs));
		hat_node_path_poke(addr_hs, dst.data());

		NodeAddress addr(root);
		for(auto i : dst) {
			addr = addr.getAddressOfChild(i);
		}

		return addr;
	}

	// ------------------------------------------------------------ Runtime
	// Apparently GHC does no longer support calling `hs_exit` from a destructor.
	// Because of this the runtime is initialized and de-initialized by the static
	// instance `rt` of this class.
	class Runtime {
	  public:
		Runtime() { hs_init(0, nullptr); }

		~Runtime() { hs_exit(); }
	} rt;

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

extern "C" {

	using insieme::analysis::ArithmeticSet;
	using namespace insieme::analysis::haskell;

	arithmetic::Value* hat_mk_arithmetic_value(Context* ctx_c, const size_t* addr_hs, size_t length_hs) {
		assert_true(ctx_c) << "hat_mk_arithmetic_value called without context";
		// build NodeAddress
		NodeAddress addr(ctx_c->getRoot());
		for(size_t i = 0; i < length_hs; i++) {
			addr = addr.getAddressOfChild(addr_hs[i]);
		}

		return new arithmetic::Value(addr.as<ExpressionPtr>());
	}

	arithmetic::Product::Factor* hat_mk_arithemtic_factor(arithmetic::Value* value_c, int exponent) {
		auto ret = new pair<arithmetic::Value, int>(*value_c, exponent);
		delete value_c;
		return ret;
	}

	arithmetic::Product* hat_mk_arithmetic_product(const arithmetic::Product::Factor** factors_c, size_t length) {
		auto ret = new arithmetic::Product();
		for(size_t i = 0; i < length; i++) {
			*ret *= arithmetic::Product(factors_c[i]->first, factors_c[i]->second);
			delete factors_c[i];
		}
		return ret;
	}

	arithmetic::Formula::Term* hat_mk_arithmetic_term(arithmetic::Product* term_c, int64_t coeff) {
		auto ret = new pair<arithmetic::Product, arithmetic::Rational>(*term_c, arithmetic::Rational(coeff));
		delete term_c;
		return ret;
	}

	arithmetic::Formula* hat_mk_arithmetic_formula(const arithmetic::Formula::Term** terms_c, size_t length) {
		auto ret = new arithmetic::Formula();
		for(size_t i = 0; i < length; i++) {
			*ret += arithmetic::Formula(terms_c[i]->first, terms_c[i]->second);
			delete terms_c[i];
		}
		return ret;
	}

	ArithmeticSet* hat_mk_arithmetic_set(const arithmetic::Formula** formulas_c, int length) {
		if(length < 0) {
			return new ArithmeticSet(ArithmeticSet::getUniversal());
		}

		auto ret = new ArithmeticSet();
		for(int i = 0; i < length; i++) {
			ret->insert(*formulas_c[i]);
			delete formulas_c[i];
		}
		return ret;
	}

}
