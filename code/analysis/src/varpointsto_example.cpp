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

#include "insieme/analysis/datalog/varpointsto_example.h"

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/varpointsto_example.h"


namespace insieme {
namespace analysis {
namespace datalog {

using AnalysisBase = framework::AnalysisBase<souffle::Sf_varpointsto_example>;


namespace {

	class Analysis : public AnalysisBase {
	public:
		Analysis() : AnalysisBase() {}

		VarpointstoExample::alias_t getAlias() {
			using rel_type = decltype(rel_alias);

			VarpointstoExample::alias_t res;

			#define symtab_get_num(i) (int32_t) cur[i]
			#define symtab_get_str(i) symTable.resolve(cur[i])

			for (const rel_type::tuple_type &cur : rel_alias) {
				// There's no need for an inner loop like in 'printCSV',
				// since we know the format beforehand.
				// Hint: symTab in generated code, 0=num, 1=str
				res.emplace_back(symtab_get_str(0), symtab_get_str(1));
			}

			#undef symtab_get_num
			#undef symtab_get_str

			return res;
		}

		void printAlias() {
			auto res = getAlias();
			std::cout << "Varpointsto Alias result: " << res.size() << " elements" << std::endl;
			for (std::tuple<str_t,str_t> line : res) {
				std::cout << std::get<0>(line) << " \t"
				          << std::get<1>(line) << std::endl;
			}
		}
	};

} // end anonymous namespace


VarpointstoExample::~VarpointstoExample() {
	delete static_cast<Analysis*>(analysis);
}

void VarpointstoExample::run() {
	auto analysis = new Analysis;
	analysis->extractFacts(rootNode);
	analysis->run();
	this->analysis = analysis;
}

VarpointstoExample::alias_t VarpointstoExample::getAlias() {
	return static_cast<Analysis*>(analysis)->getAlias();
}

void VarpointstoExample::printAlias() {
	static_cast<Analysis*>(analysis)->printAlias();
}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
