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

#include "insieme/core/checks/literal_checks.h"

#include <boost/regex.hpp>

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace checks {


	namespace {

		// the flags to be passed to the regex construction
		#define FLAGS (boost::regex::flag_type)(boost::regex::optimize | boost::regex::ECMAScript)

		// some static regex expressions to be utilized by literal format checks
		boost::regex floatRegex		(R"((0)|((([1-9][0-9]*)|0)\.[0-9]+[fF]))", FLAGS);
		boost::regex doubleRegex	(R"((0)|((([1-9][0-9]*)|0)\.[0-9]+))", FLAGS);

		boost::regex charRegex		(R"('\\?.')", FLAGS);
		boost::regex stringRegex	(R"("(\\.|[^\\"])*")", FLAGS);

		boost::regex signedRegex	(R"((-?(0|[1-9][0-9]*))(l|ll)?)", FLAGS);
		boost::regex unsignedRegex	(R"((0|[1-9][0-9]*)u?(l|ll)?)", FLAGS);

	}


	OptionalMessageList LiteralFormatCheck::visitLiteral(const LiteralAddress& address) {
		const auto& basic = address->getNodeManager().getLangBasic();
		IRBuilder builder(address->getNodeManager());

		// the result list
		OptionalMessageList res;

		// the check depends on the type
		TypePtr type = address.as<LiteralPtr>()->getType();
		LiteralPtr lit = address;

		const string& value = address->getStringValue();

		// a utility to register an error
		auto addError = [&]() {
			add(res, Message(address,
					EC_FORMAT_INVALID_LITERAL,
					format("Invalid format for %s literal: %s", toString(*type), value),
					Message::ERROR));
		};

		// check booleans
		if (basic.isBool(type)) {
			if (lit != basic.getTrue() && lit != basic.getFalse()) {
				addError();
			}
			return res;
		}

		// check pattern of specific types
		boost::regex* pattern = 0;

		if (basic.isReal4(type)) 		pattern = &floatRegex;
		if (basic.isReal8(type)) 		pattern = &doubleRegex;
		if (basic.isChar(type))  		pattern = &charRegex;
//		if (basic.isString(type)) 		pattern = &stringRegex;
		if (basic.isSignedInt(type))	pattern = &signedRegex;
		if (basic.isUnsignedInt(type))	pattern = &unsignedRegex;

		// check pattern
		if (pattern && !boost::regex_match(value, *pattern)) {
			addError();
			return res;
		}

		// check value range
		if (basic.isSignedInt(type)) {
			int64_t min = 0;
			int64_t max = 0;

			if (basic.isInt1(type)) {
				min = std::numeric_limits<int8_t>::min();
				max = std::numeric_limits<int8_t>::max();
			} else if (basic.isInt2(type)) {
				min = std::numeric_limits<int16_t>::min();
				max = std::numeric_limits<int16_t>::max();
			} else if (basic.isInt4(type)) {
				min = std::numeric_limits<int32_t>::min();
				max = std::numeric_limits<int32_t>::max();
			} else if (basic.isInt8(type)) {
				min = std::numeric_limits<int64_t>::min();
				max = std::numeric_limits<int64_t>::max();
			} else {
				assert(false && "Unsupported signed integer type encountered!");
			}

			if (!basic.isInt8(type) && *value.rbegin() == 'l') {
				addError();
				return res;
			}

			int64_t num = utils::numeric_cast<int64_t>(value);
			if (!(min <= num && num <= max)) {
				add(res, Message(address,
						EC_FORMAT_INVALID_LITERAL,
						format("Literal out of range for type %s literal: %s", toString(*type), value),
						Message::ERROR)
				);
				return res;
			}

		} else if (basic.isUnsignedInt(type)) {
			uint64_t max = 0;

			if (basic.isUInt1(type)) {
				max = std::numeric_limits<uint8_t>::max();
			} else if (basic.isUInt2(type)) {
				max = std::numeric_limits<uint16_t>::max();
			} else if (basic.isUInt4(type)) {
				max = std::numeric_limits<uint32_t>::max();
			} else if (basic.isUInt8(type)) {
				max = std::numeric_limits<uint64_t>::max();
			} else {
				assert(false && "Unsupported signed integer type encountered!");
			}

			if (!basic.isUInt8(type) && *value.rbegin() == 'l') {
				addError();
				return res;
			}

			if (!(utils::numeric_cast<uint64_t>(value) <= max)) {
				add(res, Message(address,
						EC_FORMAT_INVALID_LITERAL,
						format("Literal out of range for type %s literal: %s", toString(*type), value),
						Message::ERROR)
				);
			}
		}

		// done
		return res;
	}

} // end namespace check
} // end namespace core
} // end namespace insieme
