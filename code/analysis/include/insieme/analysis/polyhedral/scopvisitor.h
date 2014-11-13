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

#include <stack>

#include "insieme/analysis/func_sema.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/scopregion.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/set_utils.h"

using namespace insieme::core;
using namespace insieme::core::lang;

using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

namespace insieme { namespace analysis { namespace polyhedral {

struct ScopVisitor : public IRVisitor<IterationVector, Address> {
	std::vector<NodeAddress>& scopList;
	std::vector<NodeAddress>  subScops;

	/** Stack utilized to keep track of statements which are inside a SCoP.
	because not all the compound statements of the IR are annotated by a SCoPRegion annotation,
	we need to have a way to collect statements which can be inside nested scopes. **/
	typedef std::stack<scop::ScopRegion::StmtVect> RegionStmtStack;
	RegionStmtStack regionStmts;

	ScopVisitor(std::vector<NodeAddress>& scopList);
	IterationVector markAccessExpression(const ExpressionPtr& expr);
	RefList collectRefs(IterationVector& iterVec, const StatementAddress& body);
	static AffineConstraintPtr fromConstraint(IterationVector& iterVect, const arithmetic::Constraint& constraint);
		// <- called in the Piecewise version of extractFrom
	static AffineConstraintPtr extractFrom(IterationVector& iterVec, const Piecewise& pw,
										   const ExpressionPtr& compExpr, const ConstraintType ct);
		// <-- called from extractFromEx, buildStridedDomain
	static AffineConstraintPtr extractFrom(IterationVector& iterVec, const ExpressionPtr& expr,
										   const ExpressionPtr& trg, const ConstraintType& ct);
		// <- used in buildStridedDomain and in extractFromCondition
	template <class BoundType> AffineConstraintPtr buildStridedDomain(NodeManager& mgr,
		IterationVector& ret, const VariablePtr& iter, const BoundType& lb, const BoundType& ub, const Formula& stride);
		// <- used in visitStmt, visitForStmt

	IterationVector visitStmt        (              NodeAddress  addr);
	static IterationDomain extractFromCondition(IterationVector& iv, const ExpressionPtr& cond); // only used in visitIfStmt
	IterationVector visitIfStmt      (const       IfStmtAddress& ifStmt);
	IterationVector visitSwitchStmt  (const   SwitchStmtAddress& switchStmt);
	IterationVector visitForStmt     (const      ForStmtAddress& forStmt);
	IterationVector visitWhileStmt   (const    WhileStmtAddress& whileStmt);
	IterationVector visitCompoundStmt(const CompoundStmtAddress& compStmt);
	IterationVector visitMarkerStmt  (const   MarkerStmtAddress& mark);
	IterationVector visitMarkerExpr  (const   MarkerExprAddress& mark);
	IterationVector visitLambda      (const       LambdaAddress& lambda);
	IterationVector visitCallExpr    (const     CallExprAddress& callExpr);
	IterationVector visitBreakStmt   (const    BreakStmtAddress& breakStmt);
	IterationVector visitContinueStmt(const ContinueStmtAddress& contStmt);
	IterationVector visitReturnStmt  (const   ReturnStmtAddress& retStmt);
	IterationVector visitProgram     (const      ProgramAddress& prog);
	IterationVector visitNode        (const         NodeAddress& node);
};

}}}
