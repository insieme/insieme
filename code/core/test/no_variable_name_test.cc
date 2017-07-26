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
 */

#include <gtest/gtest.h>

#include <memory>
#include <map>
#include <iostream>
#include <algorithm>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/printable.h"

using std::map;
using std::pair;
using std::string;
using std::vector;

// -------------- define a simple grammar exposing bounded variables -----------------
//
//   F = Var | F + F | Ex.F
//
// -----------------------------

template <typename T>
T max(T a, T b) {
	return (a > b) ? a : b;
}

struct Formula;
typedef std::shared_ptr<Formula> FormulaPtr;

/**
 * A link forming a connection between a formula and its sub-formulas.
 * In addition to a conventional pointer, the link is also mapping free and
 * bounded variables from the parent to the free variables of the child node.
 */
struct Link {
	/**
	 * The references sub-formula.
	 */
	FormulaPtr sub;

	/**
	 * The mapping of the free (positive) and bound (negative) variables of
	 * the parent to the free variables of the sub-formula.
	 */
	vector<int> mapping;

	Link(const FormulaPtr& sub, const vector<int>& mapping) : sub(sub), mapping(mapping) {}

	template <typename... T>
	Link(const FormulaPtr& sub, T... ids)
	    : sub(sub), mapping(toVector<int>(0, ids...)) {}

	bool operator==(const Link& other) const {
		//			return *sub == *other.sub && mapping == other.mapping;
		return mapping == other.mapping;
	}

	bool operator!=(const Link& other) const {
		return !(*this == other);
	}

	bool operator<(const Link& other) const {
		// lexicographical order ignoring negative values (bound variables)
		return lexicographical_compare(mapping.begin(), mapping.end(), other.mapping.begin(), other.mapping.end(),
		                               [](int a, int b) { return max(0, a) < max(0, b); });
	}

	bool operator<=(const Link& other) const {
		return *this < other || *this == other;
	}
};


enum Kind { NT_Var, NT_And, NT_Exist };

string printFormula(const Formula& f);

struct Formula : public insieme::utils::Printable {
	const Kind kind;

	const unsigned bound;
	const unsigned free;
	const vector<Link> children;

	template <typename... Links>
	Formula(Kind kind, unsigned bound, unsigned free, const Links&... links)
	    : kind(kind), bound(bound), free(free), children(toVector<Link>(links...)){};

	virtual ~Formula(){};

	const Link& getLink(unsigned index) const {
		return children[index];
	}

	const FormulaPtr& getChild(unsigned index) const {
		return getLink(index).sub;
	}

	bool operator==(const Formula& other) const {
		return kind == other.kind && children == other.children;
	}

	bool operator!=(const Formula& other) const {
		return !(*this == other);
	}

	virtual std::ostream& printTo(std::ostream& out) const {
		return out << printFormula(*this);
	}
};

struct Variable : public Formula {
	Variable() : Formula(NT_Var, 0, 1) {}
};

struct And : public Formula {
	FormulaPtr left;
	FormulaPtr right;

	And(unsigned freeVars, const Link& left, const Link& right) : Formula(NT_And, 0, freeVars, left, right), left(getChild(0)), right(getChild(1)){};
};

struct Exist : public Formula {
	FormulaPtr sub;

	Exist(unsigned freeVars, const Link& sub) : Formula(NT_Exist, 1, freeVars, sub), sub(sub.sub){};
};


// ------ constructor -----------

FormulaPtr var() {
	return std::make_shared<Variable>();
}

FormulaPtr lAnd(unsigned freeVars, const Link& left, const Link& right) {
	assert_true(left <= right);
	return std::make_shared<And>(freeVars, left, right);
}

FormulaPtr exist(unsigned freeVars, const Link& sub) {
	return std::make_shared<Exist>(freeVars, sub);
}

// ------ utilities ---------------

namespace {

	vector<char> initNames(unsigned numVars) {
		vector<char> res(numVars + 1);
		for(unsigned i = 1; i <= numVars; i++) {
			res[i] = 'a' + (i - 1);
		}
		return res;
	}
}

class VarNames {
	const vector<char> vars;

	VarNames(const vector<char>& vars) : vars(vars) {}

  public:
	VarNames(const unsigned numFreeVars) : vars(initNames(numFreeVars)) {}

	char operator[](unsigned id) const {
		return vars[id];
	}

	VarNames map(const Formula& parent, unsigned index) const {
		const FormulaPtr& sub = parent.getChild(index);
		unsigned numVars = sub->free;
		unsigned freshCounter = vars.size() - 1;
		vector<char> res(numVars + 1);
		const vector<int>& list = parent.getLink(index).mapping;
		for(unsigned i = 1; i <= numVars; i++) {
			int cur = list[i];
			if(cur < 0) {
				res[i] = 'a' + (freshCounter++);
			} else {
				res[i] = vars[cur];
			}
		}
		return VarNames(res);
	}

	const vector<char>& getVars() const {
		return vars;
	}
};

std::ostream& operator<<(std::ostream& out, const VarNames& names) {
	return out << join(",", names.getVars().begin() + 1, names.getVars().end());
}


string printFormula(const Formula* f, const VarNames& names) {
	if(dynamic_cast<const Variable*>(f)) { return string(1, names[1]); }
	if(const And* cur = dynamic_cast<const And*>(f)) {
		return printFormula(&*(cur->left), names.map(*cur, 0)) + " & " + printFormula(&*(cur->right), names.map(*cur, 1));
	}

	if(const Exist* cur = dynamic_cast<const Exist*>(f)) {
		VarNames subNames = names.map(*cur, 0);

		// get name of bound variable
		char bound = 'x';
		const vector<int>& list = cur->getLink(0).mapping;
		for(unsigned i = 1; i < list.size(); i++) {
			if(list[i] == -1) {
				bound = subNames[i];
				break;
			}
		}
		std::ostringstream ss;
		ss << "E" << string(1, bound) << ".(" << printFormula(&*(cur->sub), subNames) << ")";
		return ss.str();
	}

	return std::string();
}

string printFormula(const FormulaPtr f) {
	return printFormula(*f);
}

string printFormula(const Formula& f) {
	return printFormula(&f, VarNames(f.free));
}


TEST(Tryout, NameFreeTerms) {
	FormulaPtr x = var();
	FormulaPtr y = var();

	EXPECT_EQ(*x, *y);

	FormulaPtr f;

	f = x;
	EXPECT_EQ("a", printFormula(f));

	f = y;
	EXPECT_EQ("a", printFormula(f));

	f = lAnd(2, Link(x, 1), Link(y, 2));
	EXPECT_EQ("a & b", printFormula(f));

	// the order is important ... (not allowed since mapping of x > mapping of y
	// f = lAnd(2, Link(x, 2), Link(y, 1));
	// EXPECT_EQ("b & a", printFormula(f));

	// ... the actual variables not
	f = lAnd(2, Link(y, 1), Link(x, 2));
	EXPECT_EQ("a & b", printFormula(f));

	// ... hence, they even may be shared
	f = lAnd(2, Link(x, 1), Link(x, 2));
	EXPECT_EQ("a & b", printFormula(f));

	// => different variables may be the same ...
	f = lAnd(1, Link(x, 1), Link(y, 1));
	EXPECT_EQ("a & a", printFormula(f));


	// --- bound variables ---

	FormulaPtr ab = lAnd(2, Link(x, 1), Link(y, 2));
	EXPECT_EQ("a & b", printFormula(ab));

	f = exist(1, Link(ab, -1, 1));
	EXPECT_EQ("Eb.(b & a)", printFormula(f));

	f = exist(1, Link(ab, 1, -1));
	EXPECT_EQ("Eb.(a & b)", printFormula(f));

	f = exist(0, Link(f, -1));
	EXPECT_EQ("Ea.(Eb.(a & b))", printFormula(f));


	// Problem: representation is not unique!!!!
	//
	//  (a & b) should be the same as (b & a) !!
	//
	// Required: invariant ensuring left-to-right indexing of
	//	free variables. => hard to enforce!!
	// Fixed by enforcing lexicographical order on sub-nodes

	//	f = lAnd(2, Link(x, 1), Link(y, 2));
	//	EXPECT_EQ("a & b", printFormula(f));
	//
	//	// the order is important ...
	//	f = lAnd(2, Link(x, 2), Link(y, 1));
	//	EXPECT_EQ("b & a", printFormula(f));
}
