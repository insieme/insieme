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

#include "insieme/utils/annotation.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/ir_expressions.h"

// #include "boost/optional.hpp"

#include <memory.h>

#define DEFINE_TYPE(Type) \
	class Type; \
	typedef std::shared_ptr<Type> Type##Ptr;

namespace insieme {

namespace xml {
class XmlElement;
}

namespace frontend {
namespace omp {

DEFINE_TYPE(BaseAnnotation);
DEFINE_TYPE(Annotation);
DEFINE_TYPE(Reduction);
DEFINE_TYPE(Schedule);
DEFINE_TYPE(Collapse);
DEFINE_TYPE(Default);
DEFINE_TYPE(For);
DEFINE_TYPE(Single);
DEFINE_TYPE(Task);
DEFINE_TYPE(TaskWait);
DEFINE_TYPE(Parallel);
DEFINE_TYPE(ParallelFor);
DEFINE_TYPE(Barrier);
DEFINE_TYPE(Critical);
DEFINE_TYPE(Master);
DEFINE_TYPE(Flush);

/**
 * It implements the annotation node which is attached to the insieme IR for OpenMP directives
 *
 * As multiple OpenMP directives can be attached to the same node, even with the same key, i.e.:
 *
 * #pragma barrier
 * #pragma flush
 * #pragma barrier
 * i++
 *
 * The omp::BaseAnnotation node will contains a list of omp pragmas which are associated to the IR node.
 */
class BaseAnnotation : public utils::CompoundAnnotation< omp::Annotation , core::NodeAnnotation > {
public:
	static const string NAME;
    static const utils::StringKey<BaseAnnotation> KEY;

    BaseAnnotation(const utils::CompoundAnnotation< omp::Annotation >::AnnotationList& annotationList):
    	utils::CompoundAnnotation< omp::Annotation , core::NodeAnnotation >(annotationList) { }

    const utils::AnnotationKey* getKey() const { return &KEY; }
	const std::string& getAnnotationName() const { return NAME; }

	const std::string toString() const;
	// for annotation compatibility
	virtual std::ostream& printTo(std::ostream& out) const { return out << "OMPAnnotation " << toString(); }

	virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
		// always copy the annotation
		assert(&*ptr == this && "Annotation pointer should reference this annotation!");
		after->addAnnotation(ptr);
		return true;
	}

private:
	AnnotationList annotationList;
};

/**
 * This is the root class for OpenMP annotations, be aware that this is not an IR Annotation (see OmpBaseAnnotation).
 */
class Annotation {
public:
	virtual std::ostream& dump(std::ostream& out) const { return out; }
};

class Barrier: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "barrier"; }
};

/**
 * Holds a list of identifiers, because omp statements can refer to global (or static)
 * variables the vector will hold both VariablePtr or MemebrExpressions.
 */
typedef std::vector<core::ExpressionPtr> VarList;
typedef std::shared_ptr<VarList> VarListPtr;

class Reduction {
public:
	// operator = + or - or * or & or | or ^ or && or ||
	enum Operator { PLUS, MINUS, STAR, AND, OR, XOR, LAND, LOR };

	Reduction(const Operator& op, const VarListPtr& vars): op(op), vars(vars) { }
	const Operator& getOperator() const { return op; }
	const VarList& getVars() const { assert(vars); return *vars; }

	std::ostream& dump(std::ostream& out) const {
		return out << "reduction(" << opToStr(op) << ": " << join(",", *vars) << ")";
	}

	static std::string opToStr(Operator op) {
		switch(op) {
		case PLUS: 	return "+";
		case MINUS: return "-";
		case STAR: 	return "*";
		case AND: 	return "&";
		case OR:	return "|";
		case XOR:	return "^";
		case LAND:	return "&&";
		case LOR:	return "||";
		}
		assert(false && "Operator doesn't exist");
	}

private:
	const Operator op;
	VarListPtr vars;
};

/**
 * Represents the OpenMP Schedule clause that may appears in for and parallelfor.
 * schedule( static | dynamic | guided | auto | runtime, [expression] )
 */
class Schedule {
public:
	enum Kind { STATIC, DYNAMIC, GUIDED, AUTO, RUNTIME };

	Schedule(const Kind& kind, const core::ExpressionPtr& chunkExpr): kind(kind), chunkExpr(chunkExpr) { }

	const Kind& getKind() const { return kind; }
	bool hasChunkSizeExpr() const { return static_cast<bool>(chunkExpr); }
	const core::Expression& getChunkSizeExpr() const { assert(hasChunkSizeExpr()); return *chunkExpr; }

	std::ostream& dump(std::ostream& out) const {
		out << "schedule(" << kindToStr(kind);
		if(hasChunkSizeExpr())
			out << ", " << *chunkExpr;
		return out << ")";
	}

	static std::string kindToStr(Kind op) {
		switch(op) {
		case STATIC: 	return "static";
		case DYNAMIC: 	return "dynamic";
		case GUIDED: 	return "guided";
		case AUTO: 		return "auto";
		case RUNTIME: 	return "runtime";
		}
		assert(false && "Scheduling kind doesn't exist");
	}
private:
	Kind kind;
	core::ExpressionPtr chunkExpr;
};

/**
 * Represents the OpenMP Default clause that may appears in for and parallelfor.
 * default( shared | none )
 */
class Default {
public:
	enum Kind { SHARED, NONE };

	Default(const Kind& mode): mode(mode) { }
	const Kind& getMode() const { return mode; }

	std::ostream& dump(std::ostream& out) const {
		return out << "default(" << modeToStr(mode) << ")";
	}

	static std::string modeToStr(Kind op) {
		switch(op) {
		case SHARED: 	return "shared";
		case NONE: 		return "none";
		}
		assert(false && "Mode doesn't exist");
	}

private:
	Kind mode;
};

/**
 * OpenMP 'master' clause
 */
class Master: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "master"; }
};

/**
 * Represent clauses which are
 */
class ForClause {
protected:
	VarListPtr			lastPrivateClause;
	SchedulePtr			scheduleClause;
	core::ExpressionPtr	collapseExpr;
	bool 				noWait;
public:
	ForClause( const VarListPtr& lastPrivateClause, const SchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, bool noWait) :
		lastPrivateClause(lastPrivateClause), scheduleClause(scheduleClause), collapseExpr(collapseExpr), noWait(noWait) { }

	bool hasLastPrivate() const { return static_cast<bool>(lastPrivateClause); }
	const VarList& getLastPrivate() const { assert(hasLastPrivate()); return *lastPrivateClause; }

	bool hasSchedule() const { return static_cast<bool>(scheduleClause); }
	const Schedule& getSchedule() const { assert(hasSchedule()); return *scheduleClause; }

	bool hasCollapse() const { return static_cast<bool>(collapseExpr); }
	const core::Expression& getCollapse() const { assert(hasCollapse()); return *collapseExpr; }

	bool hasNoWait() const { return noWait; }

	std::ostream& dump(std::ostream& out) const;
};

class SharedParallelAndTaskClause {
protected:
	core::ExpressionPtr	ifClause;
	DefaultPtr			defaultClause;
	VarListPtr			sharedClause;
public:
	SharedParallelAndTaskClause(const core::ExpressionPtr& ifClause, const DefaultPtr& defaultClause, const VarListPtr& sharedClause) :
		ifClause(ifClause), defaultClause(defaultClause), sharedClause(sharedClause) { }

	bool hasIf() const { return static_cast<bool>(ifClause); }
	const core::Expression& getIf() const { assert(hasIf()); return *ifClause; }

	bool hasDefault() const { return static_cast<bool>(defaultClause); }
	const Default& getDefault() const { assert(hasDefault()); return *defaultClause; }

	bool hasShared() const { return static_cast<bool>(sharedClause); }
	const VarList& getShared() const { assert(hasShared()); return *sharedClause; }

	std::ostream& dump(std::ostream& out) const;
};

class ParallelClause: public SharedParallelAndTaskClause {
protected:
	core::ExpressionPtr numThreadClause;
	VarListPtr			copyinClause;
public:
	ParallelClause(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const DefaultPtr& defaultClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause):
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause),
			numThreadClause(numThreadClause), copyinClause(copyinClause) { }

	bool hasNumThreads() const { return static_cast<bool>(numThreadClause); }
	const core::ExpressionPtr& getNumThreads() const { assert(hasNumThreads()); return numThreadClause; }

	bool hasCopyin() const { return static_cast<bool>(copyinClause); }
	const VarList& getCopyin() const { assert(hasCopyin()); return *copyinClause; }

	std::ostream& dump(std::ostream& out) const;
};

class CommonClause {
protected:
	VarListPtr	privateClause;
	VarListPtr	firstPrivateClause;
public:
	CommonClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause):
			privateClause(privateClause), firstPrivateClause(firstPrivateClause) { }

	bool hasPrivate() const { return static_cast<bool>(privateClause); }
	const VarList& getPrivate() const { assert(hasPrivate()); return *privateClause; }

	bool hasFirstPrivate() const { return static_cast<bool>(firstPrivateClause); }
	const VarList& getFirstPrivate() const { assert(hasFirstPrivate()); return *firstPrivateClause; }

	std::ostream& dump(std::ostream& out) const;
};


/**
 * Interface to enable common access to data sharing clauses
 */
class DatasharingClause : public CommonClause {
public:
	DatasharingClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause):
			CommonClause(privateClause, firstPrivateClause) { }

	virtual bool hasReduction() const = 0;
	virtual const Reduction& getReduction() const = 0;
};

/**
 * OpenMP 'parallel' clause
 */
class Parallel: public DatasharingClause, public Annotation, public ParallelClause {
	ReductionPtr reductionClause;
public:
	Parallel(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const ReductionPtr& reductionClause) :
			DatasharingClause(privateClause, firstPrivateClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'for' clause
 */
class For: public DatasharingClause, public Annotation, public ForClause {
	ReductionPtr reductionClause;
public:
	For(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& lastPrivateClause,
		const ReductionPtr& reductionClause,
		const SchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr,
		bool noWait) :
			DatasharingClause(privateClause, firstPrivateClause),
			ForClause(lastPrivateClause, scheduleClause, collapseExpr, noWait), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'parallel for' clause
 */
class ParallelFor: public Annotation, public CommonClause, public ParallelClause, public ForClause {
protected:
	ReductionPtr reductionClause;
public:
	ParallelFor(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const ReductionPtr& reductionClause,
		const VarListPtr& lastPrivateClause,
		const SchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr, bool noWait) :
			CommonClause(privateClause, firstPrivateClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause),
			ForClause(lastPrivateClause, scheduleClause, collapseExpr, noWait), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	ParallelPtr toParallel() const {
		return std::make_shared<Parallel>(ifClause, numThreadClause, defaultClause, privateClause, 
			firstPrivateClause, sharedClause, copyinClause, reductionClause);
	}
	ForPtr toFor() const {
		// do not duplicate stuff already handled in parallel
		return std::make_shared<For>(/*private*/VarListPtr(), /*firstprivate*/VarListPtr(), lastPrivateClause, 
			/*reduction*/ReductionPtr(), scheduleClause, collapseExpr, noWait);
	}

	std::ostream& dump(std::ostream& out) const;
};

class SectionClause {
	VarListPtr		lastPrivateClause;
	ReductionPtr	reductionClause;
	bool 			noWait;

public:
	SectionClause(const VarListPtr& lastPrivateClause,
		const ReductionPtr& reductionClause,
		bool noWait) : lastPrivateClause(lastPrivateClause), reductionClause(reductionClause), noWait(noWait) { }

	bool hasLastPrivate() const { return static_cast<bool>(lastPrivateClause); }
	const VarList& getLastPrivate() const { assert(hasLastPrivate()); return *lastPrivateClause; }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	bool hasNoWait() const { return noWait; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'sections' clause
 */
class Sections: public Annotation, public CommonClause, public SectionClause {

public:
	Sections(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& lastPrivateClause,
		const ReductionPtr& reductionClause,
		bool noWait) :
			CommonClause(privateClause, firstPrivateClause),
			SectionClause(lastPrivateClause, reductionClause, noWait) { }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'parallel sections' clause
 */
class ParallelSections: public Annotation, public CommonClause, public ParallelClause, public SectionClause {
public:
	ParallelSections(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const ReductionPtr& reductionClause,
		const VarListPtr& lastPrivateClause,
		bool noWait) :
			CommonClause(privateClause, firstPrivateClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause),
			SectionClause(lastPrivateClause, reductionClause, noWait) { }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'section' clause
 */
class Section: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "section"; }
};

/**
 * OpenMP 'single' clause
 */
class Single: public Annotation, public CommonClause {
	VarListPtr	copyPrivateClause;
	bool 		noWait;
public:
	Single(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& copyPrivateClause,
		bool noWait) :
			CommonClause(privateClause, firstPrivateClause),
			copyPrivateClause(copyPrivateClause), noWait(noWait) { }

	bool hasCopyPrivate() const { return static_cast<bool>(copyPrivateClause); }
	const VarList& getCopyPrivate() const { assert(hasCopyPrivate()); return *copyPrivateClause; }

	bool hasNoWait() const { return noWait; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'task' clause
 */
class Task: public DatasharingClause, public Annotation, public SharedParallelAndTaskClause {
	bool 	untied;
	Reduction dummy;
public:
	Task(const core::ExpressionPtr& ifClause,
		bool untied,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause) :
			DatasharingClause(privateClause, firstPrivateClause),
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause), untied(untied), dummy(Reduction::PLUS, VarListPtr()) { }

	bool hasUntied() const { return untied; }
	
	bool hasReduction() const { return false; }
	const Reduction& getReduction() const { assert(false); return dummy; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'taskwait' clause
 */
class TaskWait: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "task wait"; }
};

/**
 * OpenMP 'atomic' clause
 */
class Atomic: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "atomic"; }
};

/**
 * OpenMP 'critical' clause
 */
class Critical: public Annotation {
	std::string name;

public:
	Critical(const std::string& name): name(name) { }

	bool hasName() const { return !name.empty(); }
	const std::string& getName() const { assert(hasName()); return name; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'ordered' clause
 */
class Ordered: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "ordered"; }
};

/**
 * OpenMP 'flush' clause
 */
class Flush: public Annotation {
	VarListPtr varList;
public:
	Flush(const VarListPtr& varList): varList(varList) { }

	bool hasVarList() const { return static_cast<bool>(varList); }
	const VarList& getVarList() const { assert(hasVarList()); return *varList; }

	std::ostream& dump(std::ostream& out) const;
};

/**
 * OpenMP 'threadprivate' clause
 */
struct ThreadPrivate: public Annotation {
	std::ostream& dump(std::ostream& out) const;
};

} // End omp namespace
} // End frontend namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::frontend::omp::Annotation& ann);
}
