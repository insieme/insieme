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

#include "annotation.h"
#include "expressions.h"
#include <memory.h>

#define DEFINE_TYPE(Type) \
	class Type; \
	typedef std::shared_ptr<Type> Type##Ptr;

namespace insieme {
namespace frontend {
namespace omp {
namespace annotation {

DEFINE_TYPE(OmpAnnotation);
DEFINE_TYPE(OmpReduction);
DEFINE_TYPE(OmpSchedule);
DEFINE_TYPE(OmpCollapse);
DEFINE_TYPE(OmpDefault);
DEFINE_TYPE(OmpFor);

class OmpAnnotation : public core::Annotation {
public:
    static const core::StringKey<OmpAnnotationPtr> KEY;

    OmpAnnotation() : core::Annotation() { }
    const core::AnnotationKey* getKey() const { return &KEY; }
};

class OmpBarrier: public OmpAnnotation {
public:
	OmpBarrier() : OmpAnnotation() { }
};

/**
 * Holds a list of identifiers
 */
typedef std::vector<core::VarExprPtr> VarList;
typedef std::shared_ptr<VarList> VarListPtr;

class OmpReduction {
	std::string op;
	VarListPtr vars;
public:
	OmpReduction(const std::string& op, const VarListPtr& vars): op(op), vars(vars) { }
	const std::string& getOperator() const { return op; }
	const VarListPtr& getVars() const { return vars; }
};

class OmpSchedule {
public:
	enum Kind { STATIC, DYNAMIC, GUIDED, AUTO, RUNTIME };

	OmpSchedule(const Kind& kind, const core::ExpressionPtr& chunkExpr): kind(kind), chunkExpr(chunkExpr) { }
	const Kind& getKind() const { return kind; }
	const core::ExpressionPtr& getChunkSizeExpr() const { return chunkExpr; }
private:
	Kind kind;
	core::ExpressionPtr chunkExpr;
};

class OmpDefault {
public:
	enum Kind { SHARED, NONE };

	OmpDefault(const Kind& mode): mode(mode) { }

	const Kind& getMode() const { return mode; }
private:
	Kind mode;
};

class OmpMaster: public OmpAnnotation {
public:
	OmpMaster() : OmpAnnotation() { }
};

class OmpForImpl {
	VarListPtr			lastPrivateClause;
	OmpSchedulePtr		scheduleClause;
	core::ExpressionPtr	collapseExpr;
	bool 				noWait;
public:
	OmpForImpl( const VarListPtr& lastPrivateClause, const OmpSchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, bool noWait) :
		lastPrivateClause(lastPrivateClause), scheduleClause(scheduleClause), collapseExpr(collapseExpr), noWait(noWait) { }

	bool hasLastPrivate() { return static_cast<bool>(lastPrivateClause); }
	const VarListPtr& getLastPrivate() { return lastPrivateClause; }

	bool hasSchedule() { return static_cast<bool>(scheduleClause); }
	const OmpSchedulePtr& getSchedule() { return scheduleClause; }

	bool hasCollapse() { return static_cast<bool>(collapseExpr); }
	const core::ExpressionPtr& getCollapse() { return collapseExpr; }

	bool hasNoWait() { return noWait; }
};

class OmpParallelImpl {
	core::ExpressionPtr	ifClause;
	core::ExpressionPtr numThreadClause;
	OmpDefaultPtr		defaultClause;
	VarListPtr			sharedClause;
	VarListPtr			copyinClause;
public:
	OmpParallelImpl(const core::ExpressionPtr& ifClause, const core::ExpressionPtr& numThreadClause,
		const OmpDefaultPtr& defaultClause, const VarListPtr& sharedClause, const VarListPtr& copyinClause):
			ifClause(ifClause), numThreadClause(numThreadClause), defaultClause(defaultClause), sharedClause(sharedClause), copyinClause(copyinClause) { }

	bool hasIf() { return static_cast<bool>(ifClause); }
	const core::ExpressionPtr& getIf() { return ifClause; }

	bool hasNumThreads() { return static_cast<bool>(numThreadClause); }
	const core::ExpressionPtr& getNumThreads() { return numThreadClause; }

	bool hasDefault() { return static_cast<bool>(defaultClause); }
	const OmpDefaultPtr& getDefault() { return defaultClause; }

	bool hasShared() { return static_cast<bool>(sharedClause); }
	const VarListPtr& getShared() { return sharedClause; }

	bool hasCopyin() { return static_cast<bool>(copyinClause); }
	const VarListPtr& getCopyin() { return copyinClause; }
};

class OmpCommonImpl {
	VarListPtr			privateClause;
	VarListPtr			firstPrivateClause;
	OmpReductionPtr		reductionClause;

public:
	OmpCommonImpl(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause, const OmpReductionPtr& reductionClause):
		privateClause(privateClause), firstPrivateClause(firstPrivateClause), reductionClause(reductionClause) { }

	bool hasPrivate() { return static_cast<bool>(privateClause); }
	const VarListPtr& getPrivate() { return privateClause; }

	bool hasFirstPrivate() { return static_cast<bool>(firstPrivateClause); }
	const VarListPtr& getFirstPrivate() { return firstPrivateClause; }

	bool hasReduction() { return static_cast<bool>(reductionClause); }
	const OmpReductionPtr& getReduction() { return reductionClause; }
};

/**
 * OpenMP 'parallel' clause
 */
class OmpParallel: public OmpAnnotation, public OmpCommonImpl, public OmpParallelImpl {
public:
	OmpParallel(const core::ExpressionPtr& ifClause, const core::ExpressionPtr& numThreadClause,
			const OmpDefaultPtr& defaultClause, const VarListPtr& privateClause, const VarListPtr& firstPrivateClause,
			const VarListPtr& sharedClause, const VarListPtr& copyinClause, const OmpReductionPtr& reductionClause) :
		OmpCommonImpl(privateClause, firstPrivateClause, reductionClause),
		OmpParallelImpl(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause) { }
};

/**
 * OpenMP 'for' clause
 */
class OmpFor: public OmpAnnotation, public OmpCommonImpl, public OmpForImpl {
	VarListPtr			privateClause;
	VarListPtr			firstPrivateClause;
	OmpReductionPtr		reductionClause;

public:
	OmpFor(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause,
			const VarListPtr& lastPrivateClause, const OmpReductionPtr& reductionClause,
			const OmpSchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, bool noWait) :
		OmpCommonImpl(privateClause, firstPrivateClause, reductionClause),
		OmpForImpl(lastPrivateClause, scheduleClause, collapseExpr, noWait) { }
};

/**
 * OpenMP 'parallel for' clause
 */
class OmpParallelFor: public OmpAnnotation, public OmpCommonImpl, public OmpParallelImpl, public OmpForImpl {
public:
	OmpParallelFor(const core::ExpressionPtr& ifClause, const core::ExpressionPtr& numThreadClause,
				const OmpDefaultPtr& defaultClause, const VarListPtr& privateClause, const VarListPtr& firstPrivateClause,
				const VarListPtr& sharedClause, const VarListPtr& copyinClause, const OmpReductionPtr& reductionClause,
				const VarListPtr& lastPrivateClause, const OmpSchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, bool noWait) :
			OmpCommonImpl(privateClause, firstPrivateClause, reductionClause),
			OmpParallelImpl(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause),
			OmpForImpl(lastPrivateClause, scheduleClause, collapseExpr, noWait) { }
};

/**
 * OpenMP 'parallel sections' clause
 */

} // End annotation namespace
} // End omp namespace
} // End frontend namespace
} // End insieme namespace
