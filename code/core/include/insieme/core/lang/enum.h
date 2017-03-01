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

#include <boost/algorithm/string/predicate.hpp>

#include "insieme/core/lang/extension.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

namespace insieme {
namespace core {

	class IRBuilder;


namespace lang {

	class EnumExtension : public core::lang::Extension	{
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		EnumExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

		LANG_EXT_DERIVED_WITH_NAME(EnumEquals, "enum_eq", R"(
			(x : (type<'a>, 'b), y : (type<'a>, 'b)) -> bool {
				return x.1 == y.1;
			}
		)")

		LANG_EXT_DERIVED(EnumFromInt, R"(
			(t : type<(type<'a>, 'b)>, v : 'b) -> (type<'a>, 'b) {
				return (type_lit('a), v);
			}
		)")

		LANG_EXT_DERIVED(EnumToInt, R"(
			( e : (type<'a>, 'b)) -> 'b {
				return e.1;
			}
		)")

	};

	// --------------------- Utilities ----------------------------
	class EnumEntry {
		GenericTypePtr entryName;
		int64_t value;
		EnumEntry(const GenericTypePtr& name, const int64_t& val) : entryName(name), value(val) { }

	public:
		EnumEntry(const NodePtr& node);

		EnumEntry(const EnumEntry&) = default;
		EnumEntry(EnumEntry&&) = default;

		EnumEntry& operator=(const EnumEntry&) = default;
		EnumEntry& operator=(EnumEntry&&) = default;

		// --- utilities ---

		static bool isEnumEntry(const NodePtr& node);

		static GenericTypePtr create(const GenericTypePtr& name, const ExpressionPtr& value) {
			//check value is const expr
			core::arithmetic::Formula form = core::arithmetic::toFormula(value);
			assert_true(form.isConstant() && form.isInteger()) << "Enum values must be constant and of integral type.";
			auto v = form.getIntegerValue();
			return static_cast<GenericTypePtr>(EnumEntry(name, v));
		}

		operator GenericTypePtr() const;

		// --- observers and mutators ---

		const TypePtr getEnumEntryName() const {
			return entryName;
		}

		const int64_t getEnumEntryValue() const {
			return value;
		}

		string getName() const {
			return getEnumEntryName().as<core::GenericTypePtr>()->getName()->getValue();
		}

	};

	class EnumDefinition {

		GenericTypePtr enumName;
		TypePtr intType;
		std::vector<GenericTypePtr> domain;

		EnumDefinition(const GenericTypePtr& name, const TypePtr& intType, const std::vector<GenericTypePtr>& entries)
			: enumName(name), intType(intType), domain(entries) {}

	  public:
		EnumDefinition(const NodePtr& node);

		EnumDefinition(const EnumDefinition&) = default;
		EnumDefinition(EnumDefinition&&) = default;

		EnumDefinition& operator=(const EnumDefinition&) = default;
		EnumDefinition& operator=(EnumDefinition&&) = default;


		// --- utilities ---

		static bool isEnumDefinition(const NodePtr& node);

		static GenericTypePtr create(const GenericTypePtr& name, const TypePtr& intType, const std::vector<GenericTypePtr>& entries = {}) {
			return static_cast<GenericTypePtr>(EnumDefinition(name, intType, entries));
		}

		operator GenericTypePtr() const;

		// --- observers and mutators ---

		const TypePtr getEnumName() const {
			return enumName;
		}

		const TypePtr getIntType() const {
			return intType;
		}

		unsigned getNumElements() const {
			return domain.size();
		}

		const std::vector<GenericTypePtr>& getElements() const {
			return domain;
		}

	};

	/// Checks whether the given node is an enum type (a tuple with 2 elements the first of which is a enum definition type literal)
	/// or an expression of enum type
	bool isEnum(const NodePtr& node);

	/// Builds an enum tuple type for the given enum definition
	TupleTypePtr buildEnumType(const GenericTypePtr& enumDefinition);
	/// Builds an instance of a value of the given enum definition
	TupleExprPtr buildEnumValue(const GenericTypePtr& enumDefinition, const ExpressionPtr& value);

	/// Extracts the enum definition from a given enum (tuple) type
	GenericTypePtr getEnumTypeDefinition(const TypePtr& enumType);
	/// Extracts the integral type from a given enum (tuple) type
	TypePtr getEnumIntType(const TypePtr& type);
	/// Extracts the enum entry corresponding to a given value instance (tuple)
	GenericTypePtr getEnumEntry(const ExpressionPtr& enumValue);

	ExpressionPtr buildEnumToInt(const ExpressionPtr& enumExpr);
	ExpressionPtr buildEnumFromInt(const TypePtr& enumT, const ExpressionPtr& value);

} // lang
} // core
} // insieme
