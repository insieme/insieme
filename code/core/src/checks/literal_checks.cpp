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

#include "insieme/core/checks/literal_checks.h"

#include <regex>

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace checks {


	namespace {

	// the flags to be passed to the regex construction
	#define FLAGS (std::regex::flag_type)(std::regex::optimize | std::regex::ECMAScript)

		// some static regex expressions to be utilized by literal format checks
		std::regex floatRegex(R"((-?[0-9]+((\.[0-9]+)?(((E|e)(\+|-)[0-9]+)|f)?)?)|(0x[0-9]+\.[0-9]+p(\+|-)[0-9]+))", FLAGS);
		std::regex doubleRegex(R"((-?[0-9]+((\.[0-9]+)?(((E|e)(\+|-)[0-9]+))?)?)|(0x[0-9]+\.[0-9]+p(\+|-)[0-9]+))", FLAGS);
		
		// allow char literals
		std::regex signedAndCharRegex(R"(((-?(0|[1-9][0-9]*))(l|ll)?)|('\\?.'))", FLAGS);
		std::regex unsignedRegex(R"((0|[1-9][0-9]*)u?(l|ll)?)", FLAGS);
	}


	OptionalMessageList LiteralFormatCheck::visitLiteral(const LiteralAddress& address) {
		const auto& basic = address->getNodeManager().getLangBasic();
		IRBuilder builder(address->getNodeManager());

		// the result list
		OptionalMessageList res;

		// the check depends on the type
		TypePtr type = address.as<LiteralPtr>()->getType();
		LiteralPtr lit = address;

		string value = address->getStringValue();


		if(value == "this") {
			add(res, Message(address, EC_FORMAT_INVALID_LITERAL, format(" Unresolved \"this\" literal found, check translation process :%s", toString(*type)),
			                 Message::ERROR));
		}

		// a utility to register an error
		auto addError = [&](const std::string& reason) {
			add(res,
			    Message(address, EC_FORMAT_INVALID_LITERAL, format("Invalid format for %s literal: %s\n\t%s", toString(*type), value, reason), Message::ERROR));
		};

		// check type literals
		if(analysis::isTypeLiteralType(type)) {
			// make sure they still contain the value "type_literal" we used during construction
			if(lit->getValue()->getValue() != "type_literal") { addError("Invalid TypeLiteral value"); }
		}

		// check booleans
		if(basic.isBool(type)) {
			if(lit != basic.getTrue() && lit != basic.getFalse()) { addError("bool literal is not TRUE or FALSE"); }
			return res;
		}

		// check pattern of specific types
		std::regex* pattern = 0;

		if(basic.isReal4(type)) { pattern = &floatRegex; }
		if(basic.isReal8(type)) { pattern = &doubleRegex; }
		if(basic.isChar(type)) { pattern = &signedAndCharRegex; }
		if(basic.isSignedInt(type)) { pattern = &signedAndCharRegex; }
		if(basic.isUnsignedInt(type)) { pattern = &unsignedRegex; }

		// check pattern
		if(pattern && !std::regex_match(value, *pattern)) {
			addError(format("value %s does not match type %s format", value, toString(*type)));
			return res;
		}

		// check value range
		if(basic.isSignedInt(type)) {
			int64_t min = 0;
			int64_t max = 0;

			if(basic.isInt1(type)) {
				min = std::numeric_limits<int8_t>::min();
				max = std::numeric_limits<int8_t>::max();
			} else if(basic.isInt2(type)) {
				min = std::numeric_limits<int16_t>::min();
				max = std::numeric_limits<int16_t>::max();
			} else if(basic.isInt4(type)) {
				min = std::numeric_limits<int32_t>::min();
				max = std::numeric_limits<int32_t>::max();
			} else if(basic.isInt8(type)) {
				min = std::numeric_limits<int64_t>::min();
				max = std::numeric_limits<int64_t>::max();
			} else if(basic.isInt16(type) || basic.isIntInf(type) || type.as<GenericTypePtr>()->getTypeParameter(0).isa<TypeVariablePtr>()) {
				// note int16 is used for long long, and it has same values as long for this architecture
				min = std::numeric_limits<int64_t>::min();
				max = std::numeric_limits<int64_t>::max();
			} else {
				add(res,
					Message(address, EC_FORMAT_INVALID_LITERAL, format("Literal of unsupported type %s literal: %s", toString(*type), value), Message::ERROR));
				return res;
			}

			if(!(basic.isInt8(type) || basic.isInt16(type)) && *value.rbegin() == 'l') {
				addError("long modifier (l) with no long type");
				return res;
			}

			try {
				int64_t num = utils::numeric_cast<int64_t>(value);
				if(!(min <= num && num <= max)) {
					add(res, Message(address, EC_FORMAT_INVALID_LITERAL, format("Literal out of range for type %s literal: %s", toString(*type), value),
					                 Message::ERROR));
					return res;
				}
			} catch(...) {
				add(res, Message(address, EC_FORMAT_INVALID_LITERAL, format("Malformed Literal %s literal: %s", toString(*type), value), Message::ERROR));
			}

		} else if(basic.isUnsignedInt(type)) {
			uint64_t max = 0;

			if(basic.isUInt1(type)) {
				max = std::numeric_limits<uint8_t>::max();
			} else if(basic.isUInt2(type)) {
				max = std::numeric_limits<uint16_t>::max();
			} else if(basic.isUInt4(type)) {
				max = std::numeric_limits<uint32_t>::max();
			} else if(basic.isUInt8(type)) {
				max = std::numeric_limits<uint64_t>::max();
			} else if(basic.isUInt16(type) || basic.isUIntInf(type) || type.as<GenericTypePtr>()->getTypeParameter(0).isa<TypeVariablePtr>()) {
				max = std::numeric_limits<uint64_t>::max();
			} else {
				add(res,
					Message(address, EC_FORMAT_INVALID_LITERAL, format("Literal of unspecific type %s literal: %s", toString(*type), value), Message::ERROR));
				return res;
			}

			if(!(basic.isUInt8(type) || basic.isUInt16(type)) && *value.rbegin() == 'l') {
				addError("long modifier (l) with no long type");
				return res;
			}

			try {
				if(!(utils::numeric_cast<uint64_t>(value) <= max)) {
					add(res, Message(address, EC_FORMAT_INVALID_LITERAL, format("Literal out of range for type %s literal: %s", toString(*type), value),
					                 Message::ERROR));
				}
			} catch(...) {
				add(res, Message(address, EC_FORMAT_INVALID_LITERAL, format("Malformed Literal %s literal: %s", toString(*type), value), Message::ERROR));
			}
		}

		// done
		return res;
	}

} // end namespace check
} // end namespace core
} // end namespace insieme
