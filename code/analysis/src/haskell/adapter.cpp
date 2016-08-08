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

#include <cstdint>
#include <map>
#include <sstream>
#include <vector>

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary::haskell;

extern "C" {

	using insieme::analysis::ArithmeticSet;
	using insieme::analysis::haskell::StablePtr;

	// Haskell object management
	void hat_freeStablePtr(StablePtr ptr);

	// environment bracket
	void hs_init(int, char*[]);
	void hs_exit(void);

	// IR functions
	StablePtr hat_passIR(const char* dump, size_t length);
	size_t hat_IR_length(const StablePtr dump);
	void hat_IR_printTree(const StablePtr ir);
	void hat_IR_printNode(const StablePtr addr);

	// Address functions
	StablePtr hat_passAddress(const StablePtr ir, const size_t* path, size_t length);
	size_t hat_addr_length(const StablePtr addr);
	void hat_addr_toArray(const StablePtr addr, size_t* dst);

	// Analysis
	StablePtr hat_findDecl(const StablePtr var);
	int hat_checkBoolean(const StablePtr expr);
	ArithmeticSet* hat_arithmeticValue(const StablePtr expr);

}

namespace insieme {
namespace analysis {
namespace haskell {

	// ------------------------------------------------------------ StablePtr

	HSobject::HSobject(StablePtr ptr) : ptr(ptr) {}

	HSobject::~HSobject() {
		hat_freeStablePtr(ptr);
	}

	// ------------------------------------------------------------ IR

	IR::IR(std::shared_ptr<HSobject> ir, const NodePtr& original)
		: ir(ir), original(original) {}

	size_t IR::size() const {
		return hat_IR_length(ir->ptr);
	}

	void IR::printTree() const {
		hat_IR_printTree(ir->ptr);
	}

	// ------------------------------------------------------------ Address

	Address::Address(std::shared_ptr<HSobject> addr) : addr(addr) {}

	size_t Address::size() const {
		return hat_addr_length(addr->ptr);
	}

	void Address::printNode() const {
		hat_IR_printNode(addr->ptr);
	}

	// Add the address contained in the Haskell buffer to the given root node,
	// returning a proper NodeAddress.
	NodeAddress Address::toNodeAddress(const NodePtr& root) const {
		NodeAddress ret(root);
		vector<size_t> dst(size());
		hat_addr_toArray(addr->ptr, dst.data());

		for (auto i : dst) {
			ret = ret.getAddressOfChild(i);
		}

		return ret;
	}

	// ------------------------------------------------------------ Environment

	namespace detail {
		StablePtr passIR(const NodePtr& root) {
			// create a in-memory stream
			stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

			// dump IR using a binary format
			dumpIR(buffer, root);

			// get data as C string
			const string dumps = buffer.str();
			const char* dumpcs = dumps.c_str();

			// pass to Haskell
			return hat_passIR(dumpcs, dumps.size());
		}
	}

	Environment::Environment() {
		hs_init(0, nullptr);
	}

	Environment::~Environment() {
		hs_exit();
	}

	Environment& Environment::getInstance() {
		static Environment instance;
		return instance;
	}

	NodePtr Environment::getRoot() {
		return root;
	}

	void Environment::setRoot(NodePtr root) {
		this->root = root;
	}

	IR Environment::passIR(const NodePtr& root) {
		return {make_shared<HSobject>(detail::passIR(root)), root};
	}

	Address Environment::passAddress(const NodeAddress& addr, const IR& ir) {
		// check if NodeAddress is related given tree
		assert_eq(ir.original, addr.getRootNode()) << "NodeAddress' root does not match given tree";

		// empty address corresponds to root node in Haskell
		size_t length_c = addr.getDepth() - 1;
		vector<size_t> addr_c(length_c);

		for (size_t i = 0; i < length_c; i++) {
			addr_c[i] = addr.getParentAddress(length_c - 1 - i).getIndex();
		}

		return make_shared<HSobject>(hat_passAddress(ir.ir->ptr, addr_c.data(), length_c));
	}

	boost::optional<Address> Environment::findDecl(const Address& var) {
		boost::optional<Address> ret;
		if (StablePtr target_hs = hat_findDecl(var.addr->ptr)) {
			ret = make_shared<HSobject>(target_hs);
		}
		return ret;
	}

	BooleanAnalysisResult Environment::checkBoolean(const Address& expr) {
		auto res = static_cast<BooleanAnalysisResult>(hat_checkBoolean(expr.addr->ptr));
		if (res == BooleanAnalysisResult_Neither) {
			std::vector<std::string> msgs{"Boolean Analysis Error"};
			throw AnalysisFailure(msgs);
		}
		return res;
	}

	ArithmeticSet* Environment::arithmeticValue(const Address& expr) {
		return hat_arithmeticValue(expr.addr->ptr);
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

extern "C" {

	using insieme::analysis::ArithmeticSet;
	using namespace insieme::analysis::haskell;

	StablePtr hat_parseIR(const char* ircode, size_t length) {
		insieme::core::NodeManager nm;
		insieme::core::IRBuilder builder(nm);
		auto root = builder.parseStmt(std::string(ircode, length));
		return insieme::analysis::haskell::detail::passIR(root);
	}

	// ------------------------------------------------------------ Arithmetic

	arithmetic::Value* hat_arithmetic_value(const size_t* addr_hs, size_t length) {
		auto& env = Environment::getInstance();

		// build NodeAddress
		NodeAddress addr(env.getRoot());
		for(size_t i = 0; i < length; i++) {
			addr = addr.getAddressOfChild(addr_hs[i]);
		}

		return new arithmetic::Value(addr.as<ExpressionPtr>());
	}

	arithmetic::Product::Factor* hat_arithemtic_factor(arithmetic::Value* value, int exponent) {
		auto ret = new pair<arithmetic::Value, int>(*value, exponent);
		delete value;
		return ret;
	}

	arithmetic::Product* hat_arithmetic_product(const arithmetic::Product::Factor** factors, size_t length) {
		auto ret = new arithmetic::Product();
		for(size_t i = 0; i < length; i++) {
			*ret *= arithmetic::Product(factors[i]->first, factors[i]->second);
			delete factors[i];
		}
		return ret;
	}

	arithmetic::Formula::Term* hat_arithmetic_term(arithmetic::Product* term, int64_t coeff) {
		auto ret = new pair<arithmetic::Product, arithmetic::Rational>(*term, arithmetic::Rational(coeff));
		delete term;
		return ret;
	}

	arithmetic::Formula* hat_arithmetic_formula(const arithmetic::Formula::Term** terms, size_t length) {
		auto ret = new arithmetic::Formula();
		for(size_t i = 0; i < length; i++) {
			*ret += arithmetic::Formula(terms[i]->first, terms[i]->second);
			delete terms[i];
		}
		return ret;
	}

	ArithmeticSet* hat_arithmetic_set(const arithmetic::Formula** formulas, int length) {
		if(length < 0) {
			return new ArithmeticSet(ArithmeticSet::getUniversal());
		}

		auto ret = new ArithmeticSet();
		for(int i = 0; i < length; i++) {
			ret->insert(*formulas[i]);
			delete formulas[i];
		}
		return ret;
	}

}
