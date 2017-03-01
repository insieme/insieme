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
#include "insieme/backend/backend.h"

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/addons/cpp_casts.h"
#include "insieme/backend/addons/complex_type.h"
#include "insieme/backend/addons/compound_operators.h"
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/addons/io.h"
#include "insieme/backend/addons/longlong_type.h"
#include "insieme/backend/addons/asm_stmt.h"
#include "insieme/backend/addons/varargs.h"
#include "insieme/backend/addons/static_variables.h"
#include "insieme/backend/addons/comma_operator.h"

namespace insieme {
namespace backend {

	void Backend::addDefaultAddons() {
		addAddOn<addons::PointerType>();
		addAddOn<addons::CppCastsAddon>();
		addAddOn<addons::ComplexType>();
		addAddOn<addons::CompoundOps>();
		addAddOn<addons::EnumType>();
		addAddOn<addons::InputOutput>();
		addAddOn<addons::LongLongType>();
		addAddOn<addons::AsmStmt>();
		addAddOn<addons::VarArgs>();
		addAddOn<addons::StaticVariables>();
		addAddOn<addons::CommaOperator>();
	}

}
}
