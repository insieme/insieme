/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/core/arithmetic/arithmetic.h"

#include <utility>
#include <functional>
#include <algorithm>

// CUDD - the BDD library
#include <cuddObj.hh>
#include <cuddInt.h>

#include "insieme/utils/iterator_utils.h"
#include "insieme/utils/lazy.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"


namespace insieme {
namespace core {
namespace arithmetic {


	namespace {

		// Compute the Greatest Common Denominator
		unsigned gcd(unsigned a, unsigned b) {
			if (b == 0) { return a; }
			return gcd(b, a%b);
		}

		// Compute the Least Common Multiple
		unsigned lcm(unsigned a, unsigned b) {
			return a * b / gcd(a,b);
		}

		void reduce(int& numerator, unsigned &denominator) {
			unsigned GCD = gcd(abs(numerator), denominator);

			numerator = numerator/static_cast<int>(GCD);
			denominator /= GCD;

			assert(denominator != 0);
		}
	}

	Rational::Rational(int num, unsigned den) : numerator(num), denominator(den) {
		assert(den != 0 && "Denominator must be > 0");
		if (numerator == 0) {
			denominator = 1;
		} else if (denominator!=1) {
			reduce(numerator, denominator);
		}
	}


	Rational Rational::operator+(const Rational& other) const {
		unsigned LCM = lcm( abs(denominator), other.denominator );
		return Rational( numerator * (LCM/denominator) + other.numerator * (LCM/other.denominator), LCM, false);
	}

	Rational Rational::operator-(const Rational& other) const {
		unsigned LCM = lcm( abs(denominator), other.denominator );
		return Rational( numerator * (LCM/denominator) - other.numerator * (LCM/other.denominator), LCM, false);
	}


	namespace {

		inline vector<Product::Factor> getSingle(const Value& value, int exponent) {
			if (exponent == 0) {
				// do not create an entry if exponent is zero
				return toVector<Product::Factor>();
			}
			return toVector(std::make_pair(value, exponent));
		}

		template<typename ExprTy, typename Combinator, typename Extractor, typename Container>
		Container combine(const Container& a, const Container& b) {
			typedef typename Container::value_type Element;

			// instantiate the combining operator
			Combinator op;
			Extractor ext;

			// assemble the resulting elements
			Container res;

			auto it1 = a.begin();
			auto end1 = a.end();
			auto it2 = b.begin();
			auto end2 = b.end();

			// merge factors in correct order
			while (it1 != end1 && it2 != end2) {
				const Element& a = *it1;
				const Element& b = *it2;
				if (ext(a.first) == ext(b.first)) {
					ExprTy&& exp = op(a.second,b.second);
					if ( exp != ExprTy(0) ) {
						res.push_back(std::make_pair(a.first, exp));
					}
					++it1; ++it2;
				} else if (ext(a.first) < ext(b.first)) {
					res.push_back(a);
					++it1;
				} else {
					// add inverted of second element
					res.push_back(std::make_pair(b.first, op(0,b.second)));
					++it2;
				}
			}

			// copy rest of factors
			auto out = std::back_inserter(res);

			// copy A
			std::copy(it1, end1, out);

			// copy inverse B
			std::transform(it2, end2, out, [](const Element& cur) {
				Combinator op; return std::make_pair(cur.first, op(0, cur.second));
			});

			// done
			return res;
		}

		template<typename ExprTy, typename Extractor, typename Container, typename T>
		inline ExprTy findAssignedValue(const Container& list, const T& value) {
			typedef std::pair<T,ExprTy> Element;
			Extractor ext;

			// quick check
			if (list.empty()) {
				return 0;
			}

			// short cut for single-element lists
			if (list.size() == 1u) {
				if (ext(list[0].first) == ext(value)) {
					return list[0].second;
				}
				return 0;
			}

			// search for product (binary search)
			auto end = list.end();
			auto pos = std::lower_bound(list.begin(), end, std::make_pair(value,ExprTy(0)),
					[](const Element& a, const Element& b) {
						Extractor ext;
						return ext(a.first) < ext(b.first);
					}
			);

			// check whether it has been found
			if (pos != end && ext(pos->first) == ext(value)) {
				return pos->second;
			}

			// not included
			return 0;
		}

		/**
		 * Tests whether node A is less than node B. This
		 * implementation is used to order values within products, hence formulas
		 * and may not be applicable in general.
		 */
		bool lessThan(const NodePtr& a, const NodePtr& b) {
			// check for identity
			if (*a == *b) {
				return false;
			}

			// handle types (compare string representation)
			if (a->getNodeCategory() == NC_Type && b->getNodeCategory() == NC_Type) {
				return toString(*a) < toString(*b);
			}

			// extract node types
			NodeType typeA = a->getNodeType();
			NodeType typeB = b->getNodeType();

			// most important - node type
			if (typeA != typeB) {
				return typeA < typeB;
			}

			// special handling of variables
			if (typeA == core::NT_Variable) {
				const VariablePtr& varA = static_pointer_cast<const Variable>(a);
				const VariablePtr& varB = static_pointer_cast<const Variable>(b);
				return *varA < *varB;
			}

			// special handling for identifiers
			if (typeA == core::NT_StringValue) {
				const StringValuePtr& identA = static_pointer_cast<StringValuePtr>(a);
				const StringValuePtr& identB = static_pointer_cast<StringValuePtr>(b);
				return identA->getValue() < identB->getValue();
			}

			// handle remaining expressions => lexicographically
			const NodeList& listA = a->getChildList();
			const NodeList& listB = b->getChildList();
			return std::lexicographical_compare(listA.begin(), listA.end(), listB.begin(), listB.end(),
					[](const NodePtr& a, const NodePtr& b) { return lessThan(a,b); });
		}

	}


	Value::Value(const ExpressionPtr& value) : value(value) {
		if (!isValue(value)) {
			// TODO: exchange with not a value exception
			throw NotAFormulaException(value);
		}
	}

	namespace {

		bool isValueInternal(const ExpressionPtr& expr, bool topLevel=false) {

			// ---------------------------------------------------
			//  This function is recursively determining whether
			//  a given expression is something considered to
			//  be a value within a formula.
			// ---------------------------------------------------

			const lang::BasicGenerator& basic = expr->getNodeManager().getLangBasic();

			// every value has to be of an integer type
			if (topLevel && !basic.isInt(expr->getType())) {
				return false;
			}

			// all variables are values
			if (expr->getNodeType() == core::NT_Variable) {
				return true;
			}

			// all the rest has to be a call expression
			if (expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			const CallExprPtr& call = static_pointer_cast<const CallExpr>(expr);
			const ExpressionPtr& fun = call->getFunctionExpr();
			const ExpressionList& args = call->getArguments();

			// handle references
			if (basic.isRefDeref(fun)) {
				return isValueInternal(args[0]);
			}

			// handle tuples
			if (basic.isTupleRefElem(fun) || basic.isTupleMemberAccess(fun)) {
				return isValueInternal(args[0]);
			}

			// handle composites
			if (basic.isCompositeRefElem(fun) || basic.isCompositeMemberAccess(fun)) {
				return isValueInternal(args[0]);
			}

			try {

				// handle vectors
				if (basic.isVectorSubscript(fun) || basic.isVectorRefElem(fun)) {
					return isValueInternal(args[0]) && toFormula(args[1]).isConstant();
				}

				// handle arrays
				if (basic.isArraySubscript1D(fun) || basic.isArrayRefElem1D(fun)) {
					return isValueInternal(args[0]) && toFormula(args[1]).isConstant();
				}

			} catch (const NotAFormulaException& nafe) {
				// subscript was not a constant ..
				return false;
			}

			// everything else is not a value
			return false;
		}
	}


	bool Value::isValue(const ExpressionPtr& expr) {
		return isValueInternal(expr, true);
	}

	bool Value::operator<(const Value& other) const {
		return lessThan(value, other.value);
	}

	std::ostream& Value::printTo(std::ostream& out) const {
		// just use pretty printer to format value
		return out << printer::PrettyPrinter(value);
	}

	Product::Product(const VariablePtr& var, int exponent)
		: factors(getSingle(var, exponent)) {};

	Product::Product(const Value& value, int exponent)
		: factors(getSingle(value, exponent)) {};

	Product::Product(const vector<Factor>&& factors)
		: factors(factors) {};

	Product Product::operator*(const Product& other) const {
		return Product(combine<int, std::plus<int>, id<Value>>(factors, other.factors));
	}

	Product Product::operator/(const Product& other) const {
		return Product(combine<int, std::minus<int>, id<Value>>(factors, other.factors));
	}

	bool Product::operator<(const Product& other) const {

		// quick shortcut
		if (this == &other) {
			return false;
		}

		// special handling of empty factors => always the last
		if (factors.empty()) {
			return false;
		}
		if (other.factors.empty()) {
			return true;
		}

		// the rest is ordered lexicographically
		return lexicographical_compare(factors, other.factors, [](const Factor& a, const Factor& b) {
			return a.first < b.first || (a.first == b.first && a.second > b.second);
		});
	}

	Product Product::operator^(int exp) const {
		vector<Factor> ret;
	
		for_each(factors, [&] ( const Factor& cur ) {
					ret.push_back( Factor(cur.first, cur.second*exp) );
				});

		return Product( std::move(ret) );
	}

	int Product::operator[](const Value& var) const {
		return findAssignedValue<int, id<Value>>(factors, var);
	}

	std::ostream& Product::printTo(std::ostream& out) const {

		// check whether product is empty
		if (factors.empty()) {
			return out << "1";
		}

		// print individual factors
		return out << join("*", factors, [](std::ostream& out, const Factor& cur) {
			out << cur.first;
			if (cur.second != 1) {
				out << "^" << cur.second;
			}
		});
	}

	bool Product::isLinear() const {
		return factors.size() <= static_cast<std::size_t>(1) && all(factors, [](const Factor& cur) {
			return cur.second == 1;
		});
	}

	bool Product::isPolynomial() const {
		return all(factors, [](const Factor& cur) {
			return cur.second > 0;
		});
	}

	size_t Product::getDegree() const {
		// get the degree of each product
		size_t acc = 0;
		for_each(factors, [&] (const Factor& cur) { acc+=cur.second;} );
		return acc;
	}

	


	namespace {
		inline vector<Formula::Term> getSingle(const Product& product, const Rational& coefficient) {
			return toVector(std::make_pair(product, coefficient));
		}
	}

	Formula::Formula(int value) : terms((value==0)?vector<Term>():toVector(Term(Product(), Rational(value)))) {};
	
	Formula::Formula(const Rational& value)
		: terms((value.isZero())?vector<Term>():toVector(Term(Product(), value))) {};

	Formula::Formula(const Product& product, const Rational& coefficient)
		: terms(toVector(std::make_pair(product, coefficient))) {
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const core::VariablePtr& var, int exponent, const Rational& coefficient)
		: terms(toVector(std::make_pair(Product(var, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};

	Formula::Formula(const Value& value, int exponent, const Rational& coefficient)
		: terms(toVector(Term(Product(value, exponent), coefficient))) {
		assert(exponent != 0 && "Exponent must be != 0!");
		assert(!coefficient.isZero() && "Coefficient must be != 0!");
	};


	size_t Formula::getDegree() const {
		// get the degree of each product
		std::set<size_t> degrees;
		for_each(terms, [&](const Term& cur) { degrees.insert(cur.first.getDegree()); });
		if (degrees.empty()) {
			return 0;
		}
		return *degrees.rbegin(); 
	}


	std::ostream& Formula::printTo(std::ostream& out) const {

		// handle empty formula
		if (terms.empty()) { return out << "0"; }

		// print individual factors
		bool firstTerm = true;
		return out << join("", terms, [&](std::ostream& out, const Term& cur)->std::ostream& {

			bool isFirst = firstTerm;
			firstTerm = false;

			// add the + sign if necessary
			if (!isFirst && cur.second.isPositive()) {
				out << "+";
			}

			if (cur.first.isOne()) {
				return out << cur.second;
			}

			if (cur.second.isOne()) {
				return out << cur.first;
			}

			if (cur.second.isMinusOne()) {
				return out << "-" << cur.first;
			}

			return out << cur.second << "*" << cur.first;
		});
	}

	Formula Formula::operator+(const Formula& other) const {
		return Formula(combine<Rational, std::plus<Rational>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator-(const Formula& other) const {
		return Formula(combine<Rational, std::minus<Rational>, id<Product>>(terms, other.terms));
	}

	Formula Formula::operator*(const Formula& other) const {

		// compute cross-product of terms
		Formula res;
		auto range = make_product_range(terms, other.terms);
		for_range(range, [&](const std::pair<Term, Term>& cur){
			const Product& A = cur.first.first;
			const Product& B = cur.second.first;
			const Rational& coeffA = cur.first.second;
			const Rational& coeffB = cur.second.second;
			Rational&& newCoeff = coeffA * coeffB;
			if (!newCoeff.isZero()) {
				res = res + (A * B) * newCoeff;
			}
		});
		return res;
	}

	Formula Formula::operator/(const Rational& divisor) const {
		assert(!divisor.isZero() && "Division by 0 detected");
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.second = cur.second / divisor;
		});
		return res;
	}

	Formula Formula::operator/(const Product& divisor) const {
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.first = cur.first / divisor;
		});
		return res;
	}

	Formula Formula::operator/(const Term& divisor) const {
		Formula res = *this;
		for_each(res.terms, [&](Term& cur) {
			cur.first = cur.first / divisor.first;
			cur.second = cur.second / divisor.second;
		});
		return res;
	}

	bool Formula::lessThan(const Formula& other) const {
		// compare included terms lexicographically
		return this != &other && lexicographical_compare(terms, other.terms, [](const Term& a, const Term& b) {
			return a.first < b.first || (a.first == b.first && a.second > b.second);
		});
	}

	Rational Formula::operator[](const Product& product) const {
		return findAssignedValue<Rational, id<Product>>(terms, product);
	}


	bool Formula::isConstant() const {
		return isZero() || (terms.size() == static_cast<std::size_t>(1) && terms[0].first.isOne());
	}

	bool Formula::isInteger() const {
		return isConstant() && getConstantValue().isInteger();
	}

	bool Formula::isLinear() const {
		return all(terms, [](const Term& cur) {
			return cur.first.isLinear();
		});
	}

	bool Formula::isPolynomial() const {
		return all(terms, [](const Term& cur) {
			return cur.first.isPolynomial();
		});
	}

	Rational Formula::getConstantValue() const {
		assert(isConstant());
		return (terms.empty())?Rational(0):terms[0].second;
	}



	// --- Constraints ------------------------------------------------------------------------------

	namespace detail {

		typedef ::BDD CuddBDD;

		class BDDManager {

			Cudd manager;

			/**
			 * A list of equalities maintained by this manager. The position
			 * of each equality is the index of the variable used within the
			 * BDD to represent this equality.
			 */
			vector<Inequality> atomList;


		public:

			int getVarIdFor(const Inequality& inequality) {
				for(std::size_t i =0; i<atomList.size(); ++i) {
					if (atomList[i] == inequality) {
						return i;
					}
				}
				atomList.push_back(inequality);
				return atomList.size() - 1;
			}

			const Inequality* getInequalityWithId(int id) {
				if (0 <= id && id < (int)atomList.size()) {
					return &(atomList[id]);
				}
				return NULL;
			}

			Cudd& getCuddManager() {
				return manager;
			}

			const vector<Inequality>& getAtomList() const {
				return atomList;
			}

			CuddBDD getFalse() {
				return manager.bddZero();
			}

			CuddBDD getTrue() {
				return manager.bddOne();
			}

			CuddBDD getVar(int id) {
				return manager.bddVar(id);
			}

			CuddBDD getVar(const Inequality& var) {
				return manager.bddVar(getVarIdFor(var));
			}

			bool contains(const CuddBDD& bdd) {
				return bdd.manager() == &manager;
			}

		};

		BDDManagerPtr createBDDManager() {
			return std::make_shared<BDDManager>();
		}

		typedef Constraint::DNF DNF;

		class BDD : public utils::Printable {

			/**
			 * The manager maintaining the internally stored BDD.
			 */
			BDDManagerPtr manager;

			/**
			 * The BDD wrapped by this instance.
			 */
			CuddBDD bdd;

			/**
			 * A lazy-evaluated DNF representation of this BDD.
			 */
			mutable utils::Lazy<DNF> dnf;

		public:

			BDD(const BDDManagerPtr& manager, const CuddBDD& bdd)
				: manager(manager), bdd(bdd) {
				assert(manager->contains(bdd) && "Given BDD not managed by given manager!");
			};


			// -- some factory methods --

			static BDDPtr getFalseBDD(const BDDManagerPtr& manager) {
				return std::make_shared<BDD>(manager, manager->getFalse());
			}

			static BDDPtr getFalseBDD() {
				return getFalseBDD(createBDDManager());
			}

			static BDDPtr getTrueBDD(const BDDManagerPtr& manager) {
				return std::make_shared<BDD>(manager, manager->getTrue());
			}

			static BDDPtr getTrueBDD() {
				return getTrueBDD(createBDDManager());
			}

			static BDDPtr getLiteralBDD(const BDDManagerPtr& manager, const Inequality& atom) {
				if (atom.isValid()) {
					return getTrueBDD(manager);
				}
				if (atom.isUnsatisfiable()) {
					return getFalseBDD(manager);
				}
				return std::make_shared<BDD>(manager, manager->getVar(atom));
			}

			static BDDPtr getLiteralBDD(const Inequality& atom) {
				return getLiteralBDD(createBDDManager(), atom);
			}

			bool isValid() const {
				return bdd == manager->getTrue();
			}

			bool isUnsatisfiable() const {
				return bdd == manager->getFalse();
			}

			// -- support for boolean operations --

			BDD operator!() const {
				// this is simple - use the same manager + negated BDD
				return BDD(manager, !bdd);
			}

			BDD operator&&(const BDD& other) const {
				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return BDD(manager, bdd * otherBDD);
			}

			BDD operator||(const BDD& other) const {
				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return BDD(manager, bdd + otherBDD);
			}

			bool operator==(const BDD& other) const {
				// move both to the same manager
				CuddBDD otherBDD = getWithinLocalManager(other);
				return bdd == otherBDD;
			}

			bool operator!=(const BDD& other) const {
				return !(*this == other);
			}

			bool operator<(const BDD& other) const {
				return bdd < getWithinLocalManager(other);
			}

			void toDNFInternal(Constraint::DNF& res, DdNode* node, vector<Constraint::Literal> path, DdNode* zero) const {

				// terminal case - a
				DdNode* N = Cudd_Regular(node);

				// Terminal case: print the path if one has been reached
				if (cuddIsConstant(N)) {
					if (node != zero) {
						// add path to result
						res.push_back(path);
					}
					return;
				}

				// recursively search for paths to 1
				DdNode* Nv = cuddT(N);   // true path
				DdNode* Nnv = cuddE(N);  // false path
				if (Cudd_IsComplement(node)) {
					Nv = Cudd_Not(Nv);
					Nnv = Cudd_Not(Nnv);
				}

				assert(manager->getInequalityWithId(N->index) && "Index should be mapped to inequality!");
				const Inequality& atom = *manager->getInequalityWithId(N->index);

				// false path
				path.push_back(Constraint::Literal(atom, false));
				toDNFInternal(res, Nnv, path, zero);

				// true path
				path.back().second = true;
				toDNFInternal(res, Nv, path, zero);

				// clear path step
				path.pop_back();
			}

			const DNF& toDNF() const {

				// check the lazy evaluated DNF - if it is there, use it
				if (dnf.isEvaluated()) {
					return dnf.getValue();
				}

				// init result DNF
				Constraint::DNF res;

				// collect the literals by iterating through the tree
				vector<Constraint::Literal> path;
				auto zero = bdd.manager()->bddZero().getNode();
				toDNFInternal(res, bdd.getNode(), path, zero);

				// done
				return dnf.setValue(res);
			}

			std::ostream& printTo(std::ostream& out) const {

				// process constant representations
				if (isValid()) return out << "true";
				if (isUnsatisfiable()) return out << "false";

				// the rest is represented using a DNF format
				const DNF& dnf = toDNF();

				return out << join(" or ", dnf, [](std::ostream& out, const Constraint::Conjunction& cur) {
					out << "(" << join(" and ", cur, [](std::ostream& out, const Constraint::Literal& lit) {
						if (lit.second) {
							out << lit.first;
						} else {
							out << "!(" << lit.first << ")";
						}
					}) << ")";
				});
			}

		private:

			CuddBDD getWithinLocalManager(const BDD& bdd) const {

				// check whether it is already within the same manager ...
				if (bdd.manager == manager) {
					return bdd.bdd;
				}

				// migrate to local manager
				CuddBDD res = bdd.bdd.Transfer(manager->getCuddManager());

				// rename variables within result
				// TODO: use only atoms actually in use
				const vector<Inequality>& remote_atoms = bdd.manager->getAtomList();

				Cudd* mgr = &manager->getCuddManager();
				std::size_t numAtoms = remote_atoms.size();

				/**
				 * Within the new manager, the IDs of the variables need to be adapted
				 * to match the local IDs. The method SwapVariables can be used for that.
				 * However, to avoid ID capturing (e.g. by moving var 0 to var 1 and var 1
				 * to var 2 everything will end up to be 2) the src and trg IDs have to be
				 * disjoint.
				 *
				 * To guarantee this, the migration happens in two steps. First, all
				 * the variable IDs are moved by an offset outside the potential target range.
				 * In a second step the moved IDs are moved back into the target range.
				 */

				// prepare some container for the switching
				std::size_t offset = numAtoms + manager->getAtomList().size();
				DdNode* src[numAtoms];
				DdNode* trg[numAtoms];

				// 1) first, move all nodes by an offset of the size of the sum of both managers
				// this step is necessary to avoid index capturing
				{
					for(std::size_t i=0; i<numAtoms; i++) {
						src[i] = manager->getVar(i).getNode();
						trg[i] = manager->getVar(i + offset).getNode();
					}
					BDDvector srcVec(numAtoms, mgr, src);
					BDDvector trgVec(numAtoms, mgr, trg);

					res = res.SwapVariables(srcVec, trgVec);
				}

				// 2) second, move all remote variables back into the proper places
				{
					for(std::size_t i=0; i<numAtoms; i++) {
						int newId = manager->getVarIdFor(remote_atoms[i]);
						src[i] = manager->getVar(i+offset).getNode();
						trg[i] = manager->getVar(newId).getNode();
					}
					BDDvector srcVec(numAtoms, mgr, src);
					BDDvector trgVec(numAtoms, mgr, trg);

					res = res.SwapVariables(srcVec, trgVec);
				}

				// check result
				assert( toString(bdd) == toString(BDD(manager, res)) && "Error during migration!");

				// done
				return res;
			}

		};

	}


	Constraint::Constraint()
		: bdd(detail::BDD::getFalseBDD()) {}

	Constraint::Constraint(const Inequality& atom)
		: bdd(detail::BDD::getLiteralBDD(atom)) {}

	Constraint::Constraint(const detail::BDDPtr& bdd)
		: bdd(bdd) {}


	bool Constraint::isValid() const {
		return bdd->isValid();
	}

	bool Constraint::isUnsatisfiable() const {
		return bdd->isUnsatisfiable();
	}

	Constraint Constraint::operator!() const {
		return Constraint(std::make_shared<detail::BDD>(!(*bdd)));
	}

	Constraint Constraint::operator&&(const Constraint& other) const {
		return Constraint(std::make_shared<detail::BDD>(*bdd && *other.bdd));
	}

	Constraint Constraint::operator||(const Constraint& other) const {
		return Constraint(std::make_shared<detail::BDD>(*bdd || *other.bdd));
	}

	bool Constraint::operator==(const Constraint& other) const {
		return *bdd == *other.bdd;
	}

	bool Constraint::operator<(const Constraint& other) const {
		return *bdd < *other.bdd;
	}

	const Constraint::DNF& Constraint::toDNF() const {
		return bdd->toDNF();
	}

	std::ostream& Constraint::printTo(std::ostream& out) const {
		return out << *bdd;
	}



	// --- Piecewise ------------------------------------------------------------------------------


	namespace {

		typedef Piecewise::Piece Piece;

		/**
		 * A utility class supporting the proper creation of piece lists.
		 */
		class pieces_builder {

			/**
			 * The actual list of pieces.
			 */
			vector<Piece> pieces;

		public:

			void addPiece(const Constraint& constraint, const Formula& formula) {

				// skip unsatisfiable pieces => not of any value
				if (constraint.isUnsatisfiable()) {
					return;
				}

				// test whether same formula has been computed before
				auto pos = std::find_if(pieces.begin(), pieces.end(), [&](const Piece& cur) {
					return cur.second == formula;
				});
				if (pos != pieces.end()) {
					// append constraint to previous formula
					pos->first = pos->first || constraint;
					return;
				}

				// add new piece
				pieces.push_back(Piece(constraint, formula));
			}

			const vector<Piece>& getPieces() {
				// sort list before returning
				std::sort(pieces.begin(), pieces.end(), [](const Piece& a, const Piece& b) {
					return a.first < b.first;
				});
				return pieces;
			}

		};


		template<typename combinator>
		vector<Piece> combine(const vector<Piece>& a, const vector<Piece>& b) {

			// create instance of combination operator
			combinator op;

			// create a piece builder
			pieces_builder builder;

			// create all combinations ...
			for_range(make_product_range(a, b), [&](const pair<Piece,Piece>& cur) {

				// just add new piece to the builder
				builder.addPiece(
						cur.first.first && cur.second.first,		// the conjunction of the constraints
						op(cur.first.second, cur.second.second)		// the combination of the values
				);

			});

			// use builder to complete list of pieces
			return builder.getPieces();
		}


		vector<Piece> expandPiece(const Piece& piece, const Formula& elseValue) {
			if (piece.first.isValid()) {
				return toVector(piece);
			}
			if (piece.first.isUnsatisfiable() || piece.second == elseValue) {
				return toVector(Piece(Constraint::getTrue(), elseValue));
			}

			// assign 0 to all non-covered regions
			return toVector(piece, Piece(!piece.first, elseValue));
		}

	}

	Piecewise::Piecewise(const Constraint& constraint, const Formula& thenValue, const Formula& elseValue)
		: pieces(expandPiece(Piece(constraint, thenValue), elseValue)) {}

	Piecewise::Piecewise(const Piece& piece)
		: pieces(expandPiece(piece, 0)) {}


	Piecewise Piecewise::operator+(const Piecewise& other) const {
		return Piecewise(combine<std::plus<Formula>>(pieces, other.pieces));
	}

	Piecewise Piecewise::operator-(const Piecewise& other) const {
		return Piecewise(combine<std::minus<Formula>>(pieces, other.pieces));
	}

	Piecewise Piecewise::operator*(const Piecewise& other) const {
		return Piecewise(combine<std::multiplies<Formula>>(pieces, other.pieces));
	}

	Piecewise Piecewise::replace(core::NodeManager& mgr, const std::map<Value, Formula>& replacements) const {

		// quick check
		if (replacements.empty()) {
			return *this;
		}

		pieces_builder builder;

		for_each(pieces, [&](const Piece& cur) {
			builder.addPiece(
					arithmetic::replace(mgr, cur.first, replacements),
					arithmetic::replace(mgr, cur.second, replacements)
			);
		});

		// create resulting piecewise formula
		return Piecewise(builder.getPieces());
	}

	/**
	 * This method is required by the printable interface and allows
	 * instances of this class to be printed to some output stream.
	 */
	std::ostream& Piecewise::printTo(std::ostream& out) const {
		// Output format:
		// 3*v1+6*v2+6 -> if (2*v1-v2 >= 0); 2 -> if NOT(2*v1-v2 >= 0)
		return out << join("; ", pieces, [](std::ostream& out, const Piecewise::Piece& cur) {
			out << cur.second << " -> if " << cur.first;
		});
	}



////===== Constraint ================================================================================
//Piecewise::PredicatePtr normalize(const Piecewise::Predicate& c) {
//	const Piecewise::PredicateType& type = c.getType();
//
//	if ( type == Piecewise::PredicateType::EQ ||
//		 type == Piecewise::PredicateType::GE )
//	{
//		return makeCombiner(c);
//	}
//
//	if ( type == Piecewise::PredicateType::NE ) {
//		// if the contraint is !=, then we convert it into a negation
//		return not_( Piecewise::Predicate(c.getFunction(), Piecewise::PredicateType::EQ) );
//	}
//
//	Formula newF( c.getFunction() );
//	// we have to invert the sign of the coefficients
//	if ( type == Piecewise::PredicateType::LT ||
//	     type == Piecewise::PredicateType::LE )
//	{
//		newF = 0 - newF;
//	}
//	if ( type == Piecewise::PredicateType::LT ||
//		 type == Piecewise::PredicateType::GT )
//	{
//		// we have to subtract -1 to the constant part
//		newF = newF - 1;
//	}
//	return newF >= 0;
//}
//
//
//Formula toFormula(const Piecewise& pw) {
//	if (pw.empty()) { return Formula(); }
//
//	typedef utils::RawConstraintCombiner<Formula> RawPredicate;
//	typedef std::shared_ptr<const RawPredicate> RawPredicatePtr;
//
//	// The only sitation where a piecewise can be converted into a formula is when a single piece is
//	// contained and the predicate is the identity 0 == 0.
//
//	const Piecewise::Piece& p = *pw.begin();
//	if (RawPredicatePtr pred = std::dynamic_pointer_cast<const RawPredicate>(p.first) ) {
//		const Piecewise::Predicate& cons = pred->getConstraint();
//		if ( cons.getFunction() == Formula() && cons.getType() == Piecewise::PredicateType::EQ ) {
//			return p.second;
//		}
//	}
//	throw NotAFormulaException( ExpressionPtr() );
//}
//
//bool isFormula(const Piecewise& pw) {
//	try {
//		toFormula(pw);
//		return true;
//	} catch (const NotAFormulaException& e) {}
//	return false;
//}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
