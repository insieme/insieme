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

#pragma once

#include <string>

#include <boost/utility.hpp>

#include "insieme/utils/set_utils.h"
#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/lang.h"

namespace insieme {
namespace core {

	typedef utils::set::PointerSet<TypePtr> TypeSet;

	namespace lang {

		using std::string;

		class LiteralNotFoundException : std::exception {
			string msg;

		  public:
			LiteralNotFoundException(const string& lit) throw();
			~LiteralNotFoundException() throw() {}
			const char* what() const throw() {
				return msg.c_str();
			}
		};

		class BasicGenerator : boost::noncopyable {
			NodeManager& nm;
			mutable bool builtinsHaveBeenInitialized = false;
			struct BasicGeneratorImpl;
			mutable BasicGeneratorImpl* pimpl;
			class SubTypeLattice;
			mutable SubTypeLattice* subTypeLattice;

		  public:
			BasicGenerator(NodeManager& nm);
			~BasicGenerator();

			NodeManager& getNodeManager() const {
				return nm;
			}

			enum Operator {
			#define OPERATOR(_id, _str) _id,
			#include "insieme/core/lang/inspire_api/lang.def"
			};

			#define TYPE(_id, _spec)                                                                                                                           \
				TypePtr get##_id() const;                                                                                                                      \
				bool is##_id(const NodePtr& p) const;

			#define LITERAL(_id, _name, _spec)                                                                                                                 \
				LiteralPtr get##_id() const;                                                                                                                   \
				bool is##_id(const NodePtr& p) const;                                                                                                          \
				CallExprPtr isCallOf##_id(const NodePtr& p) const;

			#define DERIVED(_id, _name, _spec)                                                                                                                 \
				ExpressionPtr get##_id() const;                                                                                                                \
				bool is##_id(const NodePtr& p) const;                                                                                                          \
				CallExprPtr isCallOf##_id(const NodePtr& p) const;

			#define OPERATION(_type, _op, _name, _spec)                                                                                                        \
				LiteralPtr get##_type##_op() const;                                                                                                            \
				bool is##_type##_op(const NodePtr& p) const;

			#define DERIVED_OP(_type, _op, _name, _spec)                                                                                                       \
				ExpressionPtr get##_type##_op() const;                                                                                                         \
				bool is##_type##_op(const NodePtr& p) const;

			#define GROUP(_id, ...)                                                                                                                            \
				bool is##_id(const NodePtr& p) const;                                                                                                          \
				const vector<NodePtr>& get##_id##Group() const;

			#include "insieme/core/lang/inspire_api/lang.def"

			ExpressionPtr getBuiltIn(const std::string& name) const;
			LiteralPtr getLiteral(const string& name) const;

			/**
			 * Required to support operators on type literals.
			 */
			bool isType(const NodePtr& type) const;

			/**
			 * Required to support operators on generic types.
			 */
			bool isGen(const NodePtr& type) const;

			/**
			 * Obtains an expression representing the the requested operator for the
			 * given data type.
			 */
			ExpressionPtr getOperator(const TypePtr& type, const Operator& op) const;

			/**
			 * Obtains the operator (as defined in the lang.def file) from a literal
			 * expression obtained through the previous method (inverse procedure)
			 */
			Operator getOperator(const ExpressionPtr& lit) const;

			/**
			 * Indicates whether the builtins in this BasicGenerator have already all been parsed and marked as builtins
			 */
			bool hasBeenInitialized() const {
				return builtinsHaveBeenInitialized;
			}

			/**
			 * Sets the initialized state to true
			 */
			void setInitialized() const {
				builtinsHaveBeenInitialized = true;
			}

			// ----- type hierarchy utilities ---

			/**
			 * Two methods obtaining all direct super / sub types of the given generic types
			 */
			TypeSet getDirectSuperTypesOf(const TypePtr& type) const;
			TypeSet getDirectSubTypesOf(const TypePtr& type) const;

		  private:
			// obtains a pointer to a lazy-instantiated sub-type lattice
			const SubTypeLattice* getSubTypeLattice() const;
		};


		/**
		 * Prints the operator enumeration in a readable format.
		 */
		std::ostream& operator<<(std::ostream& out, const BasicGenerator::Operator& op);

	} // namespace lang
} // namespace core
} // namespace insieme
