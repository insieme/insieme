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

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	struct Formula : public utils::Printable, public utils::Hashable {

		typedef boost::optional<core::arithmetic::Formula> formula_type;
		formula_type formula;

		Formula() : formula() {};
		Formula(const core::arithmetic::Formula& formula) : formula(formula) {};

		bool operator==(const Formula& other) const {
			return this == &other || (!formula && !other.formula) ||
					(formula && other.formula && *formula == *other.formula);
		}

		bool operator<(const Formula& other) const {
			return (!formula && other.formula) || (other.formula && formula->lessThan(*other.formula));
		}

		operator bool() const {
			return formula;
		}

		std::ostream& printTo(std::ostream& out) const {
			if (formula) return out << *formula;
			return out << "-unknown-";
		}

		std::size_t hash() const {
			// TODO: implement hashing for formulas!
			return (formula) ? 1 : 0;	// formulas can't be hashed yet ..
		}
	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
