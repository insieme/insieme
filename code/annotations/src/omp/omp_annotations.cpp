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

#include "insieme/annotations/omp/omp_annotations.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/encoder/encoder.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/dump/annotations.h"


namespace insieme {
namespace annotations {

const string OmpObjectiveAnnotation::NAME = "OmpObjectiveAnnotation";
const utils::StringKey<OmpObjectiveAnnotation> OmpObjectiveAnnotation::KEY("OmpObjective");

void OmpObjectiveAnnotation::attach(const core::NodePtr& node, std::map<enum Parameter, core::ExpressionPtr>& weights, std::map<enum Parameter, RangeExpr>& constraints) {
	node->addAnnotation(std::make_shared<OmpObjectiveAnnotation>(weights, constraints));
}

} // namespace annotations
} // namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::OmpObjectiveAnnotation& lAnnot) {
		out << "OmpObjectiveAnnotation (" << lAnnot.getRegionId() << "):\n";
        auto eneRange = lAnnot.getConstraint(insieme::annotations::ENERGY);
        auto powRange = lAnnot.getConstraint(insieme::annotations::POWER);
        auto timRange = lAnnot.getConstraint(insieme::annotations::TIME);
        auto eneWeight = lAnnot.getWeight(insieme::annotations::ENERGY);
        auto powWeight = lAnnot.getWeight(insieme::annotations::POWER);
        auto timWeight = lAnnot.getWeight(insieme::annotations::TIME);

        out << "Energy (> " << eneRange.first << ", < " << eneRange.second << ", " << eneWeight << ") ";
        out << "Power (> " << powRange.first << ", < " << powRange.second << ", " << powWeight << ") ";
        out << "Time (> " << timRange.first << ", < " << timRange.second << ", " << timWeight << ") " << std::endl;

		return out;
	}

} // end namespace std
