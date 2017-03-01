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

#include "insieme/utils/constraint/solver.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace utils {
namespace constraint {


	TEST(Solver, ConstraintInputSetsIfElem) {
		typedef TypedSetVariable<int> Set;

		Set a = 1;
		Set b = 2;
		Set c = 3;

		// create constraint to be tested
		auto constraint = subsetIf(0, a, b, c);

		Assignment ass;

		EXPECT_TRUE(constraint->hasAssignmentDependentDependencies());

		EXPECT_EQ("[v1,v2]", toString(constraint->getInputs()));
		EXPECT_EQ("[v3]", toString(constraint->getOutputs()));
		EXPECT_EQ("[v1]", toString(constraint->getUsedInputs(ass)));

		ass[a].insert(0);
		EXPECT_EQ("[v1,v2]", toString(constraint->getUsedInputs(ass)));
	}


	TEST(Constraint, Check) {
		auto s1 = TypedSetVariable<int>(1);
		auto s2 = TypedSetVariable<int>(2);

		Assignment a;
		a[s1] = {1, 2};
		a[s2] = {1, 2, 3};

		auto c = subset(s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subset(s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = elem(3, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(0u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = elem(3, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(0u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf(3, s2, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf(3, s2, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf(3, s1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf(3, s1, s2, s1);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 5, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 1, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);
	}


	TEST(Solver, Basic) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };

		Constraints problem = {
		    elem(5, s(1)),
		    elem(6, s(1)),
		    subset(s(1), s(2)),
		    subset(s(2), s(3)),
		    subset(s(4), s(3)),

		    elem(7, s(5)),
		    subsetIf(6, s(3), s(5), s(3)),
		    subsetIfBigger(s(2), 1, s(3), s(6)),
		    subsetIfBigger(s(2), 3, s(3), s(7)),
		    subsetIfBigger(s(2), 4, s(3), s(8)),

		    subsetIfReducedBigger(s(2), 5, 0, s(3), s(9)),
		    subsetIfReducedBigger(s(2), 5, 1, s(3), s(10)),
		    subsetIfReducedBigger(s(3), 5, 1, s(3), s(11)),

		};

		auto res = solve(problem);
		EXPECT_EQ("{v1={5,6},v2={5,6},v3={5,6,7},v5={7},v6={5,6,7},v9={5,6,7},v11={5,6,7}}", toString(res));

		EXPECT_EQ("{5,6}", toString(res[s(1)])) << res;
		EXPECT_EQ("{5,6}", toString(res[s(2)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(3)])) << res;
		EXPECT_EQ("{}", toString(res[s(4)])) << res;
		EXPECT_EQ("{7}", toString(res[s(5)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(6)])) << res;
		EXPECT_EQ("{}", toString(res[s(7)])) << res;
		EXPECT_EQ("{}", toString(res[s(8)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(9)])) << res;
		EXPECT_EQ("{}", toString(res[s(10)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(11)])) << res;

		// check the individual constraints
		for(const auto& cur : problem) {
			EXPECT_TRUE(cur->check(res)) << cur;
		}
	}

	TEST(Solver, Functions) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };

		auto inc = [](const std::set<int>& a) -> std::set<int> {
			std::set<int> res;
			for(auto x : a) {
				res.insert(x + 1);
			}
			return res;
		};

		auto add = [](const std::set<int>& a, const std::set<int>& b) -> std::set<int> {
			std::set<int> res;
			for(auto x : a) {
				for(auto y : b) {
					res.insert(x + y);
				}
			}
			return res;
		};

		Constraints problem = {

		    elem(5, s(1)),
		    elem(6, s(2)),
		    elem(7, s(2)),

		    subsetUnary(s(1), s(3), inc),
		    subsetUnary(s(2), s(4), inc),

		    subsetBinary(s(1), s(2), s(5), add),
		};

		auto res = solve(problem);
		EXPECT_EQ("{v1={5},v2={6,7},v3={6},v4={7,8},v5={11,12}}", toString(res));

		EXPECT_EQ("{5}", toString(res[s(1)])) << res;
		EXPECT_EQ("{6,7}", toString(res[s(2)])) << res;
		EXPECT_EQ("{6}", toString(res[s(3)])) << res;
		EXPECT_EQ("{7,8}", toString(res[s(4)])) << res;
		EXPECT_EQ("{11,12}", toString(res[s(5)])) << res;
	}

	TEST(Solver, Lazy) {
		// lazy-evaluated faculty values
		auto resolver = [](const std::set<Variable>& sets) -> Constraints {
			Constraints res;
			for(auto cur : sets) {
				int id = cur.getID();
				if(id == 0) {
					res.add(elem(0, TypedSetVariable<int>(id)));
				} else if(id == 1 || id == 2) {
					res.add(elem(1, TypedSetVariable<int>(id)));
				} else {
					TypedSetVariable<int> a(id - 1);
					TypedSetVariable<int> b(id - 2);
					TypedSetVariable<int> r(id);
					res.add(subsetBinary(a, b, r, [](const std::set<int>& a, const std::set<int>& b) -> std::set<int> {
						std::set<int> res;
						for(int x : a)
							for(int y : b)
								res.insert(x + y);
						return res;
					}));
				}
			}
			return res;
		};

		// see whether we can compute something
		auto res = solve(TypedSetVariable<int>(4), resolver);
		//		std::cout << res << "\n";
		EXPECT_EQ("{3}", toString(res[TypedSetVariable<int>(4)]));

		// see whether we can compute something
		res = solve(TypedSetVariable<int>(46), resolver);
		//		std::cout << res << "\n";
		EXPECT_EQ("{1836311903}", toString(res[TypedSetVariable<int>(46)]));
	}


	namespace {

		struct Pair : public std::pair<int, int> {
			Pair(int a = 10, int b = 10) : std::pair<int, int>(a, b) {}
		};

		struct pair_meet_assign_op {
			bool operator()(Pair& a, const Pair& b) const {
				bool res = false;
				if(a.first > b.first) {
					a.first = b.first;
					res = true;
				}
				if(a.second > b.second) {
					a.second = b.second;
					res = true;
				}
				return res;
			}
		};

		struct pair_less_op {
			bool operator()(const Pair& a, const Pair& b) const {
				return a.first >= b.first && a.second >= b.second;
			}
		};
	}


	TEST(Solver, Lattice) {
		typedef Lattice<Pair, pair_meet_assign_op, pair_less_op> PairLattice;
		auto s = [](int id) { return TypedVariable<PairLattice>(id); };

		Constraints problem = {subset(Pair(5, 8), s(1)), subset(Pair(8, 5), s(1)), subset(Pair(5, 8), s(2)),
		                       subset(Pair(8, 5), s(3)), subset(s(2), s(4)),       subset(s(3), s(4))};

		auto res = solve(problem);
		EXPECT_EQ("{v1=(5,5),v2=(5,8),v3=(8,5),v4=(5,5)}", toString(res));

		// check the individual constraints
		for(const auto& cur : problem) {
			EXPECT_TRUE(cur->check(res)) << "Constraint: " << *cur;
		}
	}


	namespace {

		struct IncrementConstraint : public Constraint {
			TypedSetVariable<int> in;
			TypedSetVariable<int> out;

			IncrementConstraint(const TypedSetVariable<int>& in, const TypedSetVariable<int>& out)
			    : Constraint(toVector<Variable>(in), toVector<Variable>(out)), in(in), out(out) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				const auto& sin = ass[in];
				auto& sout = ass[out];

				// if there is not only one value in the in-set reset counter
				if(sin.size() != 1u) {
					sout.clear();
					sout.insert(1);
					return Constraint::Altered;
				}

				// if there is one value in the set, increment it
				int value = *sin.begin();
				if(value < 10) {
					sout.clear();
					sout.insert(value + 1);
					return Constraint::Altered;
				}

				// otherwise do nothing
				return Constraint::Unchanged;
			};
			virtual bool check(const Assignment& /*ass*/) const {
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& o) const {
				return o << in << "->" << out << "\n";
			}
			virtual std::ostream& writeDotEdge(std::ostream& o, const Assignment& /*ass*/) const {
				return writeDotEdge(o);
			}

			virtual bool hasAssignmentDependentDependencies() const {
				return false;
			}
			virtual const std::vector<Variable>& getUsedInputs(const Assignment& /*ass*/) const {
				return getInputs();
			}

			virtual std::ostream& printTo(std::ostream& o) const {
				return o << this->out << " += " << in;
			}
		};


		ConstraintPtr increment(const TypedSetVariable<int>& in, const TypedSetVariable<int>& out) {
			return std::make_shared<IncrementConstraint>(in, out);
		}
	}


	TEST(Solver, ResetConstraintsEager) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };

		Constraints problem = {subset(s(1), s(2)), subset(s(2), s(3)), increment(s(3), s(1))};

		auto res = solve(problem);
		EXPECT_EQ("{v1={10},v2={10},v3={10}}", toString(res));

		// check the individual constraints
		for(const auto& cur : problem) {
			EXPECT_TRUE(cur->check(res)) << "Constraint: " << *cur;
		}
	}

	TEST(Solver, ResetConstraintsLazy) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };

		Constraints problem = {subset(s(1), s(2)), subset(s(2), s(3)), increment(s(3), s(1))};


		// lazy-evaluated faculty values
		auto resolver = [&](const std::set<Variable>& sets) -> Constraints {
			Constraints res;
			for(auto cur : sets) {
				int id = cur.getID();
				if(id == 1) {
					res.add(increment(s(3), s(1)));
				} else if(id == 2) {
					res.add(subset(s(1), s(2)));
				} else if(id == 3) {
					res.add(subset(s(2), s(3)));
				}
			}
			return res;
		};

		// LazySolver
		auto res = solve(s(1), resolver);
		EXPECT_EQ("{v1={10},v2={10},v3={10}}", toString(res));
	}


	namespace {

		struct DynamicConstraint : public Constraint {
			TypedSetVariable<TypedSetVariable<int>> set; // the set containing elements to be aggregated
			TypedSetVariable<int> out;                   // the result set

			mutable std::vector<Variable> inputs;

			DynamicConstraint(const TypedSetVariable<TypedSetVariable<int>>& set, const TypedSetVariable<int>& out)
			    : Constraint(toVector<Variable>(set), toVector<Variable>(out), true, true), set(set), out(out) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				bool changed = false;
				const std::set<TypedSetVariable<int>>& sets = ass[set];
				for(auto cur : sets) {
					for(auto e : (const std::set<int>&)(ass[cur])) {
						changed = ass[out].insert(e).second || changed;
					}
				}
				return (changed) ? Constraint::Incremented : Constraint::Unchanged;
			};

			virtual bool check(const Assignment& ass) const {
				const std::set<int>& value = ass[out];
				const std::set<TypedSetVariable<int>>& sets = ass[set];
				for(auto cur : sets) {
					for(auto e : (const std::set<int>&)(ass[cur])) {
						if(!contains(value, e)) { return false; }
					}
				}
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				assert_not_implemented();
				return out;
			}
			virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& /*ass*/) const {
				return writeDotEdge(out);
			}

			virtual const std::vector<Variable>& getUsedInputs(const Assignment& ass) const {
				inputs.clear();
				inputs.push_back(set);
				const std::set<TypedSetVariable<int>>& sets = ass[set];
				inputs.insert(inputs.end(), sets.begin(), sets.end());
				return inputs;
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "union(all s in " << set << ") sub " << this->out;
			}
		};


		ConstraintPtr collect(const TypedSetVariable<TypedSetVariable<int>>& sets, const TypedSetVariable<int>& out) {
			return std::make_shared<DynamicConstraint>(sets, out);
		}


		typedef std::map<Variable, std::vector<ConstraintPtr>> ConstraintMap;

		struct MapResolver {
			ConstraintMap map;

		  public:
			MapResolver(const ConstraintMap& map) : map(map) {}

			Constraints operator()(const std::set<Variable>& values) const {
				Constraints res;

				for(auto value : values) {
					auto pos = map.find(value);
					if(pos != map.end()) {
						for(auto cur : pos->second) {
							res.add(cur);
						}
					}
				}
				return res;
			}
		};
	}

	TEST(Solver, DynamicDependenciesEager) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };
		auto m = [](int id) { return TypedSetVariable<TypedSetVariable<int>>(id); };

		Constraints problem = {elem(1, s(1)),
		                       elem(2, s(1)),
		                       elem(4, s(2)),
		                       elem(6, s(2)),
		                       elem(8, s(3)),
		                       elem(10, s(4)),
		                       elem(s(1), m(10)),
		                       elemIf(2, s(5), s(2), m(10)),
		                       elemIf(4, s(5), s(3), m(10)),
		                       collect(m(10), s(5))};

		auto res = solve(problem);
		EXPECT_EQ("{v10={v1,v2,v3},v1={1,2},v2={4,6},v3={8},v4={10},v5={1,2,4,6,8}}", toString(res));

		// check the individual constraints
		for(const auto& cur : problem) {
			EXPECT_TRUE(cur->check(res)) << "Constraint: " << *cur;
		}
	}

	TEST(Solver, DynamicDependenciesLazy) {
		auto s = [](int id) { return TypedSetVariable<int>(id); };
		auto m = [](int id) { return TypedSetVariable<TypedSetVariable<int>>(id); };

		ConstraintMap map;

		map[s(1)] = toVector(elem(1, s(1)), elem(2, s(1)));
		map[s(2)] = toVector(elem(4, s(2)), elem(6, s(2)));
		map[s(3)] = toVector(elem(8, s(3)));
		map[s(4)] = toVector(elem(10, s(4)));
		map[s(5)] = toVector(collect(m(10), s(5)));

		map[m(10)] = toVector(elem(s(1), m(10)), elemIf(2, s(5), s(2), m(10)), elemIf(4, s(5), s(3), m(10)));

		// LazySolver
		auto res = solve(s(5), MapResolver(map));
		EXPECT_EQ("{v10={v1,v2,v3},v1={1,2},v2={4,6},v3={8},v5={1,2,4,6,8}}", toString(res));
	}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
