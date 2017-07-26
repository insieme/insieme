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

#include "insieme/frontend/omp/omp_annotation.h"

//#include "insieme/xml/xml_utils.h"
#include <memory>

namespace insieme {
namespace frontend {
namespace omp {

	const string BaseAnnotation::NAME = "OmpAnnotation";
	const utils::StringKey<BaseAnnotation> BaseAnnotation::KEY("OpenMP");

	const std::string BaseAnnotation::toString() const {
		std::ostringstream ss;
		for(AnnotationList::const_iterator it = getAnnotationListBegin(), end = getAnnotationListEnd(); it != end; ++it) {
			(*it)->dump(ss);
			if(it + 1 != end) { ss << "\\n"; }
		}
		return ss.str();
	};

	///----- ForClause -----
	std::ostream& ForClause::dump(std::ostream& out) const {
		if(hasLastPrivate()) { out << "lastprivate(" << join(",", *lastPrivateClause) << "), "; }
		if(hasSchedule()) { scheduleClause->dump(out) << ", "; }
		if(hasCollapse()) { out << "collapse(" << *collapseExpr << "), "; }
		if(hasNoWait()) { out << "nowait, "; }
		return out;
	}

	///----- SharedParallelAndTaskClause -----
	std::ostream& SharedParallelAndTaskClause::dump(std::ostream& out) const {
		if(hasIf()) { out << "if(" << *ifClause << "), "; }
		if(hasDefault()) { defaultClause->dump(out) << ", "; }
		if(hasShared()) { out << "shared(" << join(",", *sharedClause) << "), "; }
		return out;
	}

	///----- ParallelClause -----
	std::ostream& ParallelClause::dump(std::ostream& out) const {
		SharedParallelAndTaskClause::dump(out) << ",";
		if(hasNumThreads()) { out << "num_threads(" << *numThreadClause << "), "; }
		if(hasCopyin()) { out << "copyin(" << join(",", *copyinClause) << "), "; }
		return out;
	}

	///----- CommonClause -----
	std::ostream& CommonClause::dump(std::ostream& out) const {
		if(hasPrivate()) { out << "private(" << join(",", *privateClause) << "), "; }
		if(hasFirstPrivate()) { out << "firstprivate(" << join(",", *firstPrivateClause) << "), "; }
		return out;
	}

	///----- Parallel -----
	std::ostream& Parallel::dump(std::ostream& out) const {
		out << "parallel(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		if(hasReduction()) { reductionClause->dump(out) << ", "; }
		return out << ")";
	}

	///----- For -----
	std::ostream& For::dump(std::ostream& out) const {
		out << "for(";
		CommonClause::dump(out);
		ForClause::dump(out);
		if(hasReduction()) { reductionClause->dump(out) << ", "; }
		return out << ")";
	}

	///----- ParallelFor -----
	std::ostream& ParallelFor::dump(std::ostream& out) const {
		out << "parallel for(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		ForClause::dump(out);
		if(hasReduction()) { reductionClause->dump(out) << ", "; }
		return out << ")";
	}

	///----- SectionClause -----
	std::ostream& SectionClause::dump(std::ostream& out) const {
		if(hasLastPrivate()) { out << "lastprivate(" << join(",", *lastPrivateClause) << "), "; }
		if(hasReduction()) { reductionClause->dump(out) << ", "; }
		if(hasNoWait()) { out << "nowait, "; }
		return out;
	}

	///----- Sections -----
	std::ostream& Sections::dump(std::ostream& out) const {
		out << "sections(";
		CommonClause::dump(out);
		return SectionClause::dump(out) << ")";
	}

	///----- ParallelSections -----
	std::ostream& ParallelSections::dump(std::ostream& out) const {
		out << "parallel sections(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		SectionClause::dump(out);
		return out << ")";
	}

	///----- Single -----
	std::ostream& Single::dump(std::ostream& out) const {
		out << "single(";
		CommonClause::dump(out);
		if(hasCopyPrivate()) { out << "copyprivate(" << join(",", *copyPrivateClause) << "), "; }
		if(hasNoWait()) { out << "nowait"; }
		return out << ")";
	}

	///----- Task -----
	std::ostream& Task::dump(std::ostream& out) const {
		out << "task(";
		CommonClause::dump(out);
		SharedParallelAndTaskClause::dump(out);
		if(hasUntied()) { out << "untied"; }
		return out << ")";
	}

	///----- Critical -----
	std::ostream& Critical::dump(std::ostream& out) const {
		out << "critical";
		if(hasName()) { out << "(" << name << ")"; }
		return out;
	}

	///----- Flush -----
	std::ostream& Flush::dump(std::ostream& out) const {
		out << "flush";
		if(hasVarList()) { out << "(" << join(",", *varList) << ")"; }
		return out;
	}

	///----- ThreadPrivate -----
	std::ostream& ThreadPrivate::dump(std::ostream& out) const {
		return out << "threadprivate";
	}


	void replaceVars(core::ExpressionPtr& expr, core::NodeMap map) {
		if(!expr) { return; }
		core::NodeManager& mgr = expr->getNodeManager();
		expr = core::transform::replaceAll(mgr, expr, map, core::transform::globalReplacement).as<core::ExpressionPtr>();
	}

	void replaceVars(VarListPtr& list, core::NodeMap map) {
		if(!list) { return; }
		for(core::ExpressionPtr& cur : *list) {
			core::NodeManager& mgr = cur->getNodeManager();
			cur = core::transform::replaceAll(mgr, cur, map, core::transform::globalReplacement).as<core::ExpressionPtr>();
		}
	}


} // End omp namespace
} // End frontend namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::frontend::omp::Annotation& ann) {
		ann.dump(os);
		return os;
	}
}
