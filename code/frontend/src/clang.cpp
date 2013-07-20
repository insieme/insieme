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

#include "insieme/frontend/clang.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/compiler.h"

namespace insieme {
namespace frontend {

	const unsigned ConversionSetup::DEFAULT_FLAGS = PrintDiag;

	ConversionSetup::ConversionSetup(const vector<path>& includeDirs)
		: includeDirs(includeDirs),
		  stdLibIncludeDirs(::transform(insieme::utils::compiler::getDefaultCppIncludePaths(), [](const string& cur) { return path(cur); })),
		  standard(Auto),
		  definitions(),
		  flags(DEFAULT_FLAGS) {};


	bool ConversionSetup::isCxx(const path& file) const {
		static std::set<string> CxxExtensions({ ".cpp", ".cxx", ".cc", ".C" });
		return standard == Cxx03 || (standard==Auto && ::contains(CxxExtensions, boost::filesystem::extension(file)));
	}


	// ----------- conversion ------------

	vector<tu::IRTranslationUnit> convert(core::NodeManager& manager, const vector<path>& units, const ConversionSetup& setup) {
		return ::transform(units, [&](const path& cur) { return convert(manager, cur, setup); });
	}


	// TODO: collapse this with the program class ...
	tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup) {
		// just delegate operation to converter
		Program program(manager, unit, setup);
		return conversion::Converter(manager, program).convert();
	}


} // end namespace frontend
} // end namespace insieme
