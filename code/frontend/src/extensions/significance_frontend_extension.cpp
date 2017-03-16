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
#include "insieme/frontend/extensions/significance_frontend_extension.h"

#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/pragma/matcher.h"

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/unused.h"

#include "insieme/annotations/meta_info/meta_infos.h"

using namespace insieme::frontend::pragma;
using namespace insieme::frontend;

namespace bpo = boost::program_options;

namespace insieme {
namespace frontend {
namespace extensions {

	namespace {
		pragma::node* stripEod(pragma::node* eodDelimited) {
			pragma::concat* eodConcat = dynamic_cast<pragma::concat*>(eodDelimited);
			assert_true(eodConcat != nullptr) << "Tried to strip an eod from a token sequence which is not a concat.";
			__insieme_unused auto seceod = dynamic_cast<decltype(tok::eod)*>(eodConcat->second);
			assert_true(seceod != nullptr) << "Tried to strip an eod from a token sequence not ending on eod.";
			return eodConcat->first;
		}
	}

	SignificanceFrontendExtension::SignificanceFrontendExtension() : OmpFrontendExtension() {
		// now, we adjust the pragma handling for omp task introduced by the base OMP extension

		// remove the old handler
		std::shared_ptr<PragmaHandler> originalTaskHandler;
		for(auto it = pragmaHandlers.begin(); it != pragmaHandlers.end(); ++it) {
			if((*it)->getKeyword() == "task") {
				originalTaskHandler = *it;
				pragmaHandlers.erase(it);
				break;
			}
		}

		auto ls = tok::l_square;
		auto rs = tok::r_square;
		auto lp = tok::l_paren;
		auto rp = tok::r_paren;

		auto arraySpec = [&](const string& s) {
			return kwd(s) >> lp >> tok::var[format("array_name_%s", s)] >> ls >> tok::expr[format("array_start_%s", s)] >> tok::colon
			       >> tok::expr[format("array_end_%s", s)] >> rs >> ls >> tok::expr[format("array_low_%s", s)] >> tok::semi
			       >> tok::expr[format("array_high_%s", s)] >> rs >> rp;
		};

		auto significantSpec =
		    kwd("significant") >> lp >> (kwd("expr") >> lp >> tok::expr["sig_expr"] >> rp | kwd("ratio") >> lp >> tok::expr["sig_ratio"] >> rp) >> rp;

		auto toleranceSpec = kwd("tasktolerance") >> lp >> !(kwd("taskcheck") >> lp >> tok::expr["taskcheck"] >> rp) >> tok::comma
		                     >> !(kwd("redo") >> lp >> tok::expr["redo"] >> rp) >> rp;

		// extend syntax for task by new clauses
		auto extendedTokenSequence = *stripEod(originalTaskHandler->getToken())
		                             >> *(!tok::comma >> (arraySpec("in") | arraySpec("out") | (kwd("label") >> lp >> tok::identifier["task_label"] >> rp)
		                                                  | significantSpec | toleranceSpec))
		                             >> tok::eod;

		// add a new handler which uses the old
		pragmaHandlers.push_back(std::make_shared<insieme::frontend::extensions::PragmaHandler>(
		    PragmaHandler("omp", "task", extendedTokenSequence, [originalTaskHandler](const MatchObject& object, core::NodeList nodes) {
			    // perform basic omp task generation
			    auto newNodes = originalTaskHandler->getFunction()(object, nodes);
			    // attach labels
			    insieme::annotations::significance_info sigMetainfo;
			    sigMetainfo.label = object.getString("task_label");
			    newNodes[0].as<core::MarkerStmtPtr>().getSubStatement().attachValue(sigMetainfo);
			    return newNodes;
			})));
	}

	core::tu::IRTranslationUnit SignificanceFrontendExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		OmpFrontendExtension::IRVisit(tu);

		return tu;
	}

	FrontendExtension::FlagHandler SignificanceFrontendExtension::registerFlag(bpo::options_description& options) {
		// register significance flag
		options.add_options()("ftask-significance", bpo::bool_switch(&flagActivated), "Task significance support");
		auto fHandler = [&](const ConversionJob& job) { return flagActivated; };
		return fHandler;
	}

	boost::optional<std::string> SignificanceFrontendExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		bool hasOMP = setup.hasExtension<OmpFrontendExtension>();
		if(hasOMP) {
			return boost::optional<std::string>("The task significance extension supersedes the OMP extension.\n"
			                                    "You cannot use them at the same time. (set --fopenmp=0)");
		}
		return boost::optional<std::string>();
	}

} // end namespace extensions
} // end namespace frontend
} // end namespace insieme
