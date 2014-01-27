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

#pragma once

#include <boost/operators.hpp>
#include "insieme/core/forward_decls.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A class to represent a program point - composed by the address of the currently
	 * processed statement, the execution context and whether it is before, during or
	 * after the execution of the statement.
	 */
	template<typename Context>
	struct ProgramPoint :
			public boost::equality_comparable<ProgramPoint<Context>>,
			public utils::Printable {

		/**
		 * An enumeration regarding the current state of the execution.
		 */
		enum State {
			In,				// < before the beginning of the execution
			Tmp, 			// < during the execution (e.g. after evaluation arguments but before processing calls)
			Out 			// < after completion of the execution
		};

	private:

		/**
		 * The state of the program point to be represented.
		 */
		State state;

		/**
		 * The address of the referenced program statement.
		 */
		core::StatementAddress stmt;

		/**
		 * The dynamic context of the represented statement.
		 */
		Context ctxt;

	public:

		/**
		 * Creates a new instance using the given set of fields.
		 */
		ProgramPoint(State state, const core::StatementAddress& stmt, const Context& ctxt)
			: state(state), stmt(stmt), ctxt(ctxt) {}

		/**
		 * Obtain the statement addressed by this program point.
		 */
		const core::StatementAddress& getStatement() const {
			return stmt;
		}

		/**
		 * Obtains the context referenced by this program point.
		 */
		const Context& getContext() const {
			return ctxt;
		}

		/**
		 * Obtains the processing phase of the referenced statement.
		 */
		State getState() const {
			return state;
		}

		/**
		 * Support for the == operator. Instances can be compared with other instances.
		 */
		bool operator==(const ProgramPoint& other) const {
			if (this == &other) return true;
			return state == other.state && ctxt == other.ctxt && stmt == other.stmt;
		}

		/**
		 * Support for the < operator to allow this entity to be utilized within comparison
		 * based containers (e.g. sets).
		 */
		bool operator<(const ProgramPoint& other) const {

			if (this == &other) return false;

			if (stmt < other.stmt) return true;
			if (stmt != other.stmt) return false;

			if (ctxt < other.ctxt) return true;
			if (ctxt != other.ctxt) return false;

			return state < other.state;
		}

		/**
		 * Allows this object to be printed to any output stream (in a somewhat readable manner).
		 */
		std::ostream& printTo(std::ostream& out) const {
			switch(state) {
			case In: 	out << "I"; break;
			case Tmp: 	out << "T"; break;
			case Out: 	out << "O"; break;
			}
			return out << stmt << "@" << ctxt;
		}
	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
