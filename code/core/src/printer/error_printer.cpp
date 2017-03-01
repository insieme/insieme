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
#include "insieme/core/printer/error_printer.h"

#include <boost/algorithm/string/replace.hpp>

#include "insieme/core/ir_node.h"
#include "insieme/core/annotations/error.h"

namespace insieme {
namespace core {
namespace printer {

	using namespace core::checks;

	namespace {

		static const std::string RED = "\033[31m";
		static const std::string RESET = "\033[0m";
		static const std::string GREY = "\033[37m";
		static const std::string YELLOW = "\033[33m";

		typedef std::map<NodeAddress, std::string> addr2msg;

		struct ErrorPlugin : public PrinterPlugin {
			const addr2msg& addresses;
			mutable std::vector<std::string> nextLineMessages;


			ErrorPlugin(const addr2msg& addresses) : addresses(addresses) {
				auto rootAdrr = NodeAddress(addresses.begin()->first.getRootNode());

				// add annotations for every node in the error list
				visitDepthFirst(rootAdrr, [&](const NodeAddress& cur) {
					auto match = addresses.find(cur);
					if(match != addresses.end()) { annotations::attachError(cur.getAddressedNode(), match->second); }
				});
			}

			bool covers(const NodeAddress& n) const {
				if(annotations::hasAttachedError(n)) { return true; }

				return false;
			}

			std::ostream& print(std::ostream& out, const NodeAddress& n, const std::function<void(const NodeAddress&)>& f) const {
				assert_true(annotations::hasAttachedError(n));

				// get and attach next line error
				auto error = annotations::getAttachedError(n);

				nextLineMessages.push_back(error);

				out << YELLOW;
				out << PrettyPrinter(n, PrettyPrinter::NO_LET_BINDINGS);
				out << RESET;
				return out;
			}

			std::ostream& afterNewLine(std::ostream& out) const {
				for(unsigned i = 0; i < nextLineMessages.size(); ++i) {
					out << nextLineMessages[i];
				}
				nextLineMessages.clear();
				return out;
			}

			std::ostream& afterAllDone(std::ostream& out) const {
				for(unsigned i = 0; i < nextLineMessages.size(); ++i) {
					out << "\n" << nextLineMessages[i] << "\n";
				}
				nextLineMessages.clear();
				return out;
			}
		};
	}

	IRDump dumpErrors(const MessageList& msgs, std::ostream& out) {
		return IRDump([&](std::ostream& out) -> std::ostream& {
			// for each message: split warnig/error save with address
			addr2msg errors;
			if(msgs.getAll().empty()) {
				return out << "No Error Messages to print" << std::endl;
			}
			for(auto msg : msgs.getAll()) {
				std::stringstream ss;
				auto addrs = msg.getOrigin();
				switch(msg.getType()) {
				case Message::WARNING: ss << RED << "WARNING: "; break;
				case Message::ERROR:   ss << RED << "ERROR: "; break;
				default: { assert_fail() << "what was that?"; }
				};

				ss << GREY << msg.getMessage() << RESET;

				auto body = ss.str();
				boost::replace_all(body, "\n", "\n\t");

				std::string text = "------- \n" + body + "\n-------";
				errors.insert({addrs, text});
			}

			// nothig to show, print normal
			if(msgs.getAll().empty()) { out << "the IR seems to be correct, use a pretty printer" << std::endl; }

			ErrorPlugin plug(errors);
			auto root = msgs.getAll()[0].getOrigin().getRootNode();
			PrettyPrinter print(root, plug);

			return out << print << std::endl;
		}, out);
	}

} // printer
} // core
} // insieme
