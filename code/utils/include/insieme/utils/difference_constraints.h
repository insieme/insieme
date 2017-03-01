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
#pragma once

#include <map>
#include <vector>
#include <ostream>

#include "insieme/utils/assert.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	/**
	 * A Difference-Constraint Problem is given by a set of constraints of the form
	 *
	 * 								X - Y = c
	 * and
	 * 									X = c
	 *
	 * where X and Y may be arbitrary variables and c is a (positive) integer constant.
	 * The problem is to obtain an assignment associating each variable in the list
	 * of constraints with an integer value such that all constraints are satisfied.
	 *
	 * Since for any satisfiable set of constraints there are infinite many solutions,
	 * which can be obtained by adding a constant value to all assigned values, an
	 * additional constraint is to obtain a solution with the smallest sum of all values.
	 */


	// forward declaration
	template<typename Variable>
	class DifferenceConstraints;

	/**
	 * The solution of a difference constraint problem associating every variable
	 * with an integer such that the constraints are satisfied, or, if unsatisfiable,
	 * representing indicating that the constraints are unsatisfiable.
	 */
	template<typename Variable>
	class DifferenceConstraintSolution {

		// allows the constraints class to create instances of this class
		friend class DifferenceConstraints<Variable>;

		// indicates whether this class represents an actual solution or the result of solving unsatisfiable constraints
		bool valid;

		// the solution to be represented, if valid
		std::map<Variable,int> solution;

	public:

		// creates an invalid solution
		DifferenceConstraintSolution() : valid(false) {}

		DifferenceConstraintSolution(const DifferenceConstraintSolution&) = default;
		DifferenceConstraintSolution(DifferenceConstraintSolution&&) = default;

		DifferenceConstraintSolution& operator=(const DifferenceConstraintSolution&) = default;
		DifferenceConstraintSolution& operator=(DifferenceConstraintSolution&&) = default;

		bool isValid() const {
			return valid;
		}

		operator bool() const {
			return isValid();
		}

		/**
		 * Obtains the value associated to the given variable by this solution.
		 * The value is always defined. If the requested variable did not occur in
		 * constraints this solution has been obtained from, the result is 0 -- which
		 * corresponds to a valid, minimal assignment.
		 *
		 * @param cur the variable to be looking for
		 * @return the value assigned to it
		 */
		int operator[](const Variable& cur) const {
			assert_true(isValid());
			auto pos = solution.find(cur);
			return (pos != solution.end()) ? pos->second : 0;
		}

		// Enables the printing of solutions
		friend std::ostream& operator<<(std::ostream& out, const DifferenceConstraintSolution& cur) {
			if (cur.valid) {
				return out << "{" << join(",",cur.solution, [](std::ostream& out, const std::pair<Variable,unsigned>& cur) {
					out << cur.first << "=" << cur.second;
				}) << "}";
			}
			return out << "-invalid-";
		}

	private:

		/**
		 * Constructs a valid solution based on the given assignment map.
		 */
		DifferenceConstraintSolution(std::map<Variable,int>&& solution)
			: valid(true), solution(std::move(solution)) {}

	};

	/**
	 * A class allowing to formulate difference-constraints problems as well as to solve them.
	 */
	template<typename Variable>
	class DifferenceConstraints {

		/**
		 * The internal structure of a constraint of the shape
		 *  		a - b = difference
		 */
		struct Constraint {
			Variable a;				// < the first variable
			Variable b;				// < the second variable
			unsigned difference;	// < the difference
		};

		/**
		 * The Value to b
		 */
		struct FixedValue {
			Variable a;
			int value;
		};

		/**
		 * A flag indicating whether the stated constraints are still satisfiable.
		 * This is an under-approximation and an optimization.
		 * It may be that the constraints covered are not satisfiable, but the flag states otherwise.
		 */
		bool satisfiable;

		/**
		 * Fixed-value constraints.
		 */
		std::vector<FixedValue> fixed;

		/**
		 * The constraints to be covered.
		 */
		std::vector<Constraint> constraints;

	public:

		/**
		 * Creates an empty set of constraints.
		 */
		DifferenceConstraints()
			: satisfiable(true) {}

		/**
		 * Marks this constraints to be unsatisfiable.
		 */
		void markUnsatisfiable() {
			satisfiable = false;
			fixed.clear();
			constraints.clear();
		}

		/**
		 * Determines whether the current constraints are unsatisfiable or not.
		 * The result is a under-approximation and may only be utilized for optimizations.
		 * The method may return true while the solve may still determine that the
		 * constraints are not satisfiable.
		 */
		bool isUnsatisfiable() const {
			return !satisfiable;
		}

		/**
		 * Add the a fixed value
		 *
		 * 			a = value
		 *
		 * to the set of constraints.
		 */
		void addConstraint(const Variable& a, int value) {
			if (!satisfiable) return;

			// check whether this variable is already bound
			for(const auto& cur : fixed) {
				if (cur.a == a) {
					if (cur.value != value) {
						markUnsatisfiable();
					}
					return;
				}
			}

			fixed.push_back(FixedValue{a,value});
		}


		/**
		 * Add the constraint
		 *
		 * 			a - b = difference
		 *
		 * to the set of constraints. If difference is negative, it will be reshaped
		 * into
		 *
		 * 			b - a = -difference
		 *
		 * to ensure the difference is positive. If a == b and difference != 0 the
		 * problem will become unsatisfiable.
		 */
		void addConstraint(const Variable& a, const Variable& b, int difference) {
			if (!satisfiable) return;

			// rule out unsatisfiable constraints
			if (a == b && difference != 0) {
				markUnsatisfiable();
				return;
			}

			// make sure the difference is positive
			if (difference < 0) {
				addConstraint(b,a,-difference);
			} else {
				constraints.push_back(Constraint{a,b,(unsigned)difference});
			}
		}

		/**
		 * Solves the given constraints by obtaining a solution for the given constraints
		 * which might be indicating an unsatisfiable problem.
		 */
		DifferenceConstraintSolution<Variable> solve() const {
			static const DifferenceConstraintSolution<Variable> fail;

			// quick check for satisfiability
			if (!satisfiable) return fail;

			// initializer solution
			std::map<Variable,int> solution;

			// marks bound variables
			std::map<Variable,bool> bound;

			// mark all variables within constraints as unbound
			for(const auto& cur : constraints) {
				bound[cur.a] = false;
				bound[cur.b] = false;
			}

			// fill in constant values
			for(const auto& cur : fixed) {

				// check whether already covered by propagation
				if (bound[cur.a]) {
					if (solution[cur.a] != cur.value) return fail;
					continue;
				}

				// update solution
				solution[cur.a] = cur.value;
				bound[cur.a] = true;

				// propagate to connected variables
				bool change = true;
				while(change) {
					change = false;

					// search through all constraints to deduce more information
					for(const auto& cur : constraints) {

						if (bound[cur.a] && bound[cur.b]) {

							if (solution[cur.a] - solution[cur.b] != (int)cur.difference) {
								return fail;
							}

						} else if (bound[cur.a] && !bound[cur.b]) {

							// fix value of b
							int value = solution[cur.a] - cur.difference;
							solution[cur.b] = value;

							// mark as fixed
							bound[cur.b] = true;

							// remember the change
							change = true;

						} else if (!bound[cur.a] && bound[cur.b]) {


							// fix value of a
							int value = cur.difference + solution[cur.b];
							solution[cur.a] = value;

							// mark as fixed
							bound[cur.a] = true;

							// remember the change
							change = true;

						}
					}
				}
			}

			// while there are unbound variables
			while(bound.size() != solution.size()) {

				// create a component solution
				std::map<Variable,int> localSolution;
				std::map<Variable,bool> localBound;

				int minimum = 0;

				// search for an unbound variable
				for(const auto& cur : constraints) {
					if (!bound[cur.a]) {
						// found one => fix it to zero
						localSolution[cur.a] = 0;
						localBound[cur.a] = true;
						break;
					}
				}

				// resolve all associated constraints
				bool change = true;
				while(change) {
					change = false;

					// search through all constraints to deduce more information
					for(const auto& cur : constraints) {
						if (localBound[cur.a] && localBound[cur.b]) {

							if (localSolution[cur.a] - localSolution[cur.b] != (int)cur.difference) {
								return fail;
							}

						} else if (localBound[cur.a] && !localBound[cur.b]) {

							// fix value of b
							int value = localSolution[cur.a] - cur.difference;
							localSolution[cur.b] = value;

							// keep track of the minimum
							minimum = std::min(minimum, value);

							// mark as fixed
							localBound[cur.b] = true;

							// remember the change
							change = true;

						} else if (!localBound[cur.a] && localBound[cur.b]) {


							// fix value of a
							int value = cur.difference + localSolution[cur.b];
							localSolution[cur.a] = value;

							// keep track of the minimum
							minimum = std::min(minimum, value);

							// mark as fixed
							localBound[cur.a] = true;

							// remember the change
							change = true;

						}
					}
				}


				// shift component solution to zero and merge into global solution
				for(const auto& cur : localSolution) {
					solution[cur.first] = (unsigned)(cur.second - minimum);
				}

				// also merge local bound into global bound
				for(const auto& cur : localBound) {
					if (cur.second) bound[cur.first] = true;
				}

			}

			// done
			return std::move(solution);
		}

		// enables the printing of this problem statement
		friend std::ostream& operator<<(std::ostream& out, const DifferenceConstraints& cur) {
			if (!cur.satisfiable) {
				return out << "-unsatisfiable-";
			}
			out << "{";
			out << join(",",cur.fixed,[](std::ostream& out, const FixedValue& cur) {
				out << cur.a << "=" << cur.value;
			});
			if (!cur.fixed.empty() && !cur.constraints.empty()) out << ",";
			out << join(",",cur.constraints,[](std::ostream& out, const Constraint& cur) {
				out << cur.a << "-" << cur.b << "=" << cur.difference;
			});
			out << "}";
			return out;
		}

	};


} // end namespace utils
} // end namespace insieme
