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

#include "insieme/core/annotation.h"
#include "insieme/core/expressions.h"
#include "insieme/utils/string_utils.h"

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
class BaseAnnotation : public core::CompoundAnnotation< omp::Annotation > {
public:
    static const core::StringKey<BaseAnnotation> KEY;

    BaseAnnotation(const core::CompoundAnnotation< omp::Annotation >::AnnotationList& annotationList):
    	core::CompoundAnnotation< omp::Annotation >(annotationList) { }

    const core::AnnotationKey* getKey() const { return &KEY; }
	const std::string getAnnotationName() const { return "OmpAnnotation"; }

	const std::string toString() const;

private:
	AnnotationList annotationList;
};

/**
 * This is the root class for OpenMP annotations, be aware that this is not an IR Annotation (see OmpBaseAnnotation).
 */
class Annotation {
public:
	virtual std::ostream& dump(std::ostream& out) const { return out; }
//	virtual void toXml(insieme::xml::XmlElement& elem) = 0;
};

class Barrier: public Annotation {
public:
//	void toXml(insieme::xml::XmlElement& elem);
	std::ostream& dump(std::ostream& out) const { return out << "barrier"; }
};

/**
 * Holds a list of identifiers
 */
typedef std::vector<core::VariablePtr> VarList;
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
//	void toXml(insieme::xml::XmlElement& elem);
	std::ostream& dump(std::ostream& out) const { return out << "master"; }
};

class ForClause {
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

	std::ostream& dump(std::ostream& out) const {
		if(hasLastPrivate())
			out << "lastprivate(" << join(",", *lastPrivateClause) << "), ";
		if(hasSchedule())
			scheduleClause->dump(out) << ", ";
		if(hasCollapse())
			out << "collapse(" << *collapseExpr << "), ";
		if(hasNoWait())
			out << "nowait, ";
		return out;
	}
};

class SharedParallelAndTaskClause {
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

	std::ostream& dump(std::ostream& out) const {
		if(hasIf())
			out << "if(" << *ifClause << "), ";
		if(hasDefault())
			defaultClause->dump(out) << ", ";
		if(hasShared())
			out << "shared(" << join(",", *sharedClause) << "), ";
		return out;
	}
};

class ParallelClause: public SharedParallelAndTaskClause {
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
	const core::Expression& getNumThreads() const { assert(hasNumThreads()); return *numThreadClause; }

	bool hasCopyin() const { return static_cast<bool>(copyinClause); }
	const VarList& getCopyin() const { assert(hasCopyin()); return *copyinClause; }

	std::ostream& dump(std::ostream& out) const {
		SharedParallelAndTaskClause::dump(out) << ",";
		if(hasNumThreads())
			out << "num_threads(" << *numThreadClause << "), ";
		if(hasCopyin())
			out << "copyin(" << join(",", *copyinClause) << "), ";
		return out;
	}
};

class CommonClause {
	VarListPtr	privateClause;
	VarListPtr	firstPrivateClause;
public:
	CommonClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause):
			privateClause(privateClause), firstPrivateClause(firstPrivateClause) { }

	bool hasPrivate() const { return static_cast<bool>(privateClause); }
	const VarList& getPrivate() const { assert(hasPrivate()); return *privateClause; }

	bool hasFirstPrivate() const { return static_cast<bool>(firstPrivateClause); }
	const VarList& getFirstPrivate() const { assert(hasFirstPrivate()); return *firstPrivateClause; }

	std::ostream& dump(std::ostream& out) const {
		if(hasPrivate())
			out << "private(" << join(",", *privateClause) << "), ";
		if(hasFirstPrivate())
			out << "firstprivate(" << join(",", *firstPrivateClause) << "), ";
		return out;
	}
};

/**
 * OpenMP 'parallel' clause
 */
class Parallel: public Annotation, public CommonClause, public ParallelClause {
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
			CommonClause(privateClause, firstPrivateClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "parallel(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		if(hasReduction())
			reductionClause->dump(out) << ", ";
		return out << ")";
	}
};

/**
 * OpenMP 'for' clause
 */
class For: public Annotation, public CommonClause, public ForClause {
	ReductionPtr reductionClause;
public:
	For(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& lastPrivateClause,
		const ReductionPtr& reductionClause,
		const SchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr,
		bool noWait) :
			CommonClause(privateClause, firstPrivateClause),
			ForClause(lastPrivateClause, scheduleClause, collapseExpr, noWait), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "for(";
		CommonClause::dump(out);
		ForClause::dump(out);
		if(hasReduction()) {
			reductionClause->dump(out) << ", ";
		}
		return out << ")";
	}
};

/**
 * OpenMP 'parallel for' clause
 */
class ParallelFor: public Annotation, public CommonClause, public ParallelClause, public ForClause {
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

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "parallel for(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		ForClause::dump(out);
		if(hasReduction()) {
			reductionClause->dump(out) << ", ";
		}
		return out << ")";
	}
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

	std::ostream& dump(std::ostream& out) const {
		if(hasLastPrivate())
			out << "lastprivate(" << join(",", *lastPrivateClause) << "), ";
		if(hasReduction())
			reductionClause->dump(out) << ", ";
		if(hasNoWait())
			out << "nowait, ";
		return out;
	}
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

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "sections(";
		CommonClause::dump(out);
		return SectionClause::dump(out) << ")";
	}
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

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "parallel sections(";
		CommonClause::dump(out);
		ParallelClause::dump(out);
		SectionClause::dump(out);
		return out << ")";
	}
};

/**
 * OpenMP 'section' clause
 */
class Section: public Annotation {
public:
//	void toXml(insieme::xml::XmlElement& elem);
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

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "single(";
		CommonClause::dump(out);
		if(hasCopyPrivate())
			out << "copyprivate(" << join(",", *copyPrivateClause) << "), ";
		if(hasNoWait())
			out << "nowait";
		return out << ")";
	}
};

/**
 * OpenMP 'task' clause
 */
class Task: public Annotation, public CommonClause, public SharedParallelAndTaskClause {
	bool 	untied;
public:
	Task(const core::ExpressionPtr& ifClause,
		bool untied,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause) :
			CommonClause(privateClause, firstPrivateClause),
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause), untied(untied) { }

	bool hasUntied() const { return untied; }

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "task(";
		CommonClause::dump(out);
		SharedParallelAndTaskClause::dump(out);
		if(hasUntied())
			out << "united";
		return out << ")";
	}
};

/**
 * OpenMP 'taskwait' clause
 */
class TaskWait: public Annotation {
public:
//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const { return out << "task wait"; }
};

/**
 * OpenMP 'atomic' clause
 */
class Atomic: public Annotation {
public:
//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const { return out << "atomic"; }
};

/**
 * OpenMP 'critical' clause
 */
class Critical: public Annotation {
	core::VariablePtr name;

public:
	Critical(const core::VariablePtr& name): name(name) { }

	bool hasName() const { return static_cast<bool>(name); }
	const core::Variable& getName() const { assert(hasName()); return *name; }

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "critical";
		if(hasName())
			out << "(" << *name << ")";
		return out;
	}
};

/**
 * OpenMP 'ordered' clause
 */
class Ordered: public Annotation {
public:
//	void toXml(insieme::xml::XmlElement& elem);
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

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "flush";
		if(hasVarList())
			out << "(" << join(",", *varList) << ")";
		return out;
	}
};

/**
 * OpenMP 'threadprivate' clause
 */
class ThreadPrivate: public Annotation {
	VarListPtr threadPrivateClause;
public:
	ThreadPrivate(const VarListPtr& threadPrivateClause) : threadPrivateClause(threadPrivateClause) { }

	bool hasThreadPrivate() const { return static_cast<bool>(threadPrivateClause); }
	const VarList& getThreadPrivate() const { assert(hasThreadPrivate()); return *threadPrivateClause; }

//	void toXml(insieme::xml::XmlElement& elem);

	std::ostream& dump(std::ostream& out) const {
		out << "threadprivate(";
		if(hasThreadPrivate())
			out << join(",", *threadPrivateClause);
		return out << ")";
	}
};

} // End omp namespace
} // End frontend namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::frontend::omp::Annotation& ann);
}
