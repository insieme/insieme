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

#include "insieme/core/tu/ir_translation_unit_io.h"

#include <tuple>

#include "insieme/core/ir.h"

#include "insieme/core/dump/binary_dump.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/pointer_maps.h"
#include "insieme/core/encoder/tuples.h"


namespace insieme {
namespace core {
namespace tu {


	// the type used for encoding a translation unit
	typedef std::tuple<
			IRTranslationUnit::TypeMap,
			IRTranslationUnit::FunctionMap,
			IRTranslationUnit::GlobalsList,
			IRTranslationUnit::Initializer,
	        IRTranslationUnit::EntryPointList,
			bool
		> WrapperType;


	core::ExpressionPtr toIR(core::NodeManager& manager, const IRTranslationUnit& unit) {
		return core::encoder::toIR(manager, std::make_tuple(unit.getTypes(), unit.getFunctions(), unit.getGlobals(), unit.getInitializer(),
		                                                    unit.getEntryPoints(), unit.isCXX()));
	}

	IRTranslationUnit fromIR(const core::ExpressionPtr& node) {
		// decode encoded IR
		auto values = core::encoder::toValue<WrapperType>(node);

		// build resulting translation unit
		return IRTranslationUnit(node.getNodeManager(), std::get<0>(values), std::get<1>(values), std::get<2>(values), std::get<3>(values), std::get<4>(values),
		                         std::get<5>(values));
	}


	void dump(std::ostream& out, const IRTranslationUnit& unit) {
		core::NodeManager localMgr(unit.getNodeManager());

		// encode translation unit into an IR expression
		auto encoded = toIR(localMgr, unit);

		// dump IR expression
		core::dump::binary::dumpIR(out, encoded);
	}

	IRTranslationUnit load(std::istream& in, core::NodeManager& manager) {
		// load encoded IR expression from stream
		auto encoded = core::dump::binary::loadIR(in, manager).as<core::ExpressionPtr>();
		return fromIR(encoded);
	}


} // end namespace tu
} // end namespace core
} // end namespace insieme
