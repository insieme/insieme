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

#include "insieme/frontend/extensions/while_to_for_extension.h"

#include "insieme/core/ir.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/assert.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include <exception>

namespace p = insieme::core::pattern;
namespace irp = insieme::core::pattern::irp;

#include "insieme/core/printer/pretty_printer.h"
namespace printer = insieme::core::printer;

namespace insieme {
namespace frontend {

namespace {
	class NotForException : public std::exception {
		std::string message;
	public:
		NotForException(const std::string& msg) : exception(), message(msg) {}
		~NotForException() throw() {}

		virtual const char* what() throw() { return message.c_str(); }
	};

	printer::PrettyPrinter pp(const core::NodePtr& n) { return printer::PrettyPrinter(n, printer::PrettyPrinter::NO_LET_BINDINGS); }
}

	insieme::core::ProgramPtr WhileToForPlugin::IRVisit(insieme::core::ProgramPtr& prog) {		
		auto patt = irp::whileStmt(p::var("condition", p::all(p::var("cvar", irp::variable()))), p::var("body"));

		// while statements can be IR for statements iff only one variable used in the condition is
		// altered within the statement
		irp::replaceAll(patt, prog, [&](p::AddressMatch match) {
			auto condition = match["condition"].getValue();
			auto body = match["body"].getValue();
			auto cvars = match["cvar"].getFlattened();
			insieme::utils::set::PointerSet<core::VariablePtr> cvarSet;
			for(auto cvar : cvars) {
				std::cout << "\nadding: " << *cvar.getAddressedNode() << " " << &(*cvar.getAddressedNode()) << "\n";
				for(auto existing : cvarSet) {
					std::cout << " - existing: " << *existing << " t: " << *existing->getType()
							  << "  new: " << *cvar.getAddressedNode().as<core::VariablePtr>() << " t: " << *cvar.getAddressedNode().as<core::VariablePtr>()->getType()
							  << "   equals: " << (*cvar.getAddressedNode().as<core::VariablePtr>() == *existing) << "\n";
				}
				cvarSet.insert(cvar.getAddressedNode().as<core::VariablePtr>());
			}
			std::cout << "\nWorking on while: (with condition " << pp(condition) 
				<< ", cvars: " << cvarSet << ")\n" << pp(match.getRoot()) << "\n";
			try {
				for(auto cvar : cvarSet) {
					std::cout << "- Working on cvar: " << pp(cvar) << "\n";
					auto write = p::aT(p::var("assignment", irp::assignment(p::aT(irp::atom(cvar)), p::any)));
					auto assMatch = write->matchAddress(core::NodeAddress(body.getAddressedNode()));
					if(assMatch) {
						auto asses = assMatch.get()["assignment"].getFlattened();
						if(asses.size() != 1) throw NotForException(string("More than one assignment to variable ") + toString(*cvar));
						for(auto ass : asses) {
							// check if RHS constant
						}
					}
				}
			} catch(NotForException e) {
				std::cout << "========== This While loop:\n" << pp(body) << "\n-------\n is not a For loop: " << e.what() << "\n"; 
			}
			return match.getRoot().getAddressedNode();
		} );
		
		return prog;
	}

} // frontend
} // insieme
