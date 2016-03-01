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

		LANG_EXT_DERIVED_WITH_NAME(EnumEquals, "enum_eq",
			"(x : (type<'a>, 'b), y : (type<'a>, 'b)) -> bool {	 "
			"    return x.1 == y.1;                              "
			"}                                                   "
		);

		LANG_EXT_DERIVED_WITH_NAME(EnumFromInt, "enum_from_int",
			"(t : type<(type<'a>, 'b)>, v : 'b) -> (type<'a>, 'b) {  "
			"    return (type_lit('a), v);                           "
			"}                                                       "
		);

		LANG_EXT_DERIVED_WITH_NAME(EnumToInt, "enum_to_int",
			"( e : (type<'a>, 'b)) -> 'b {  "
			"    return e.1;                "
			"}                              "
		);

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

	bool isEnum(const core::NodePtr& node);

	TupleTypePtr buildEnumType(const core::GenericTypePtr& enumDefinition);
	TypePtr getEnumTypeDefinition(const core::TypePtr& enumType);

	TypePtr getEnumIntType(const TypePtr& type);

	ExpressionPtr buildEnumToInt(const ExpressionPtr& enumExpr);
	ExpressionPtr buildEnumFromInt(const TypePtr& enumT, const ExpressionPtr& value);

} // lang
} // core
} // insieme
