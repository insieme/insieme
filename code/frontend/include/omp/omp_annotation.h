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

namespace xml {
class XmlElement;
}

namespace frontend {
namespace omp {
namespace annotation {

DEFINE_TYPE(OmpAnnotation);
DEFINE_TYPE(OmpReduction);
DEFINE_TYPE(OmpSchedule);
DEFINE_TYPE(OmpCollapse);
DEFINE_TYPE(OmpDefault);
DEFINE_TYPE(OmpFor);

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
 * The OmpBaseAnnotation node will contains a list of omp pragmas which are associated to the IR node.
 */
class OmpBaseAnnotation : public core::Annotation {
public:
	typedef std::vector<OmpAnnotationPtr> OmpAnnotationList;
    static const core::StringKey<OmpBaseAnnotation> KEY;

    OmpBaseAnnotation(const OmpAnnotationList& annotationList) : core::Annotation(), annotationList(annotationList) { }
    const core::AnnotationKey* getKey() const { return &KEY; }
	const std::string getAnnotationName() const { return "OmpAnnotation"; }

    //Iterator to access annotationList elements
    OmpAnnotationList::const_iterator getListBegin() const { return annotationList.begin(); }
    const OmpAnnotationList::const_iterator getListEnd() const { return annotationList.end(); }

private:
	OmpAnnotationList annotationList;
};


/**
 * This is the root class for OpenMP annotations, be aware that this is not an IR Annotation (see OmpBaseAnnotation).
 */
class OmpAnnotation {
public:
	virtual void toXml(insieme::xml::XmlElement& elem) = 0;
};

class OmpBarrier: public OmpAnnotation {
public:
	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * Holds a list of identifiers
 */
typedef std::vector<core::VariablePtr> VarList;
typedef std::shared_ptr<VarList> VarListPtr;

class OmpReduction {
public:
	// operator = + or - or * or & or | or ^ or && or ||
	enum Operator { PLUS, MINUS, STAR, AND, OR, XOR, LAND, LOR };
	OmpReduction(const Operator& op, const VarListPtr& vars): op(op), vars(vars) { }
	const Operator& getOperator() const { return op; }
	const VarListPtr& getVars() const { return vars; }
private:
	const Operator op;
	VarListPtr vars;
};

/**
 * Represents the OpenMP Schedule clause that may appears in for and parallelfor.
 * schedule( static | dynamic | guided | auto | runtime, [expression] )
 */
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
	void toXml(insieme::xml::XmlElement& elem);
};

class OmpForClause {
	VarListPtr			lastPrivateClause;
	OmpSchedulePtr		scheduleClause;
	core::ExpressionPtr	collapseExpr;
	bool 				noWait;
public:
	OmpForClause( const VarListPtr& lastPrivateClause, const OmpSchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, bool noWait) :
		lastPrivateClause(lastPrivateClause), scheduleClause(scheduleClause), collapseExpr(collapseExpr), noWait(noWait) { }

	bool hasLastPrivate() { return static_cast<bool>(lastPrivateClause); }
	const VarListPtr& getLastPrivate() { return lastPrivateClause; }

	bool hasSchedule() { return static_cast<bool>(scheduleClause); }
	const OmpSchedulePtr& getSchedule() { return scheduleClause; }

	bool hasCollapse() { return static_cast<bool>(collapseExpr); }
	const core::ExpressionPtr& getCollapse() { return collapseExpr; }

	bool hasNoWait() { return noWait; }

	void toXml(insieme::xml::XmlElement& elem);
};

class SharedParallelAndTaskClause {
	core::ExpressionPtr	ifClause;
	OmpDefaultPtr		defaultClause;
	VarListPtr			sharedClause;
public:
	SharedParallelAndTaskClause(const core::ExpressionPtr& ifClause, const OmpDefaultPtr& defaultClause, const VarListPtr& sharedClause) :
		ifClause(ifClause), defaultClause(defaultClause), sharedClause(sharedClause) { }

	bool hasIf() { return static_cast<bool>(ifClause); }
	const core::ExpressionPtr& getIf() { return ifClause; }

	bool hasDefault() { return static_cast<bool>(defaultClause); }
	const OmpDefaultPtr& getDefault() { return defaultClause; }

	bool hasShared() { return static_cast<bool>(sharedClause); }
	const VarListPtr& getShared() { return sharedClause; }
};

class OmpParallelClause: private SharedParallelAndTaskClause {
	core::ExpressionPtr numThreadClause;
	VarListPtr			copyinClause;
public:
	OmpParallelClause(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const OmpDefaultPtr& defaultClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause):
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause),
			numThreadClause(numThreadClause), copyinClause(copyinClause) { }

	bool hasNumThreads() { return static_cast<bool>(numThreadClause); }
	const core::ExpressionPtr& getNumThreads() { return numThreadClause; }

	bool hasCopyin() { return static_cast<bool>(copyinClause); }
	const VarListPtr& getCopyin() { return copyinClause; }
};

class OmpCommonClause {
	VarListPtr			privateClause;
	VarListPtr			firstPrivateClause;
public:
	OmpCommonClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause):
			privateClause(privateClause), firstPrivateClause(firstPrivateClause) { }

	bool hasPrivate() { return static_cast<bool>(privateClause); }
	const VarListPtr& getPrivate() { return privateClause; }

	bool hasFirstPrivate() { return static_cast<bool>(firstPrivateClause); }
	const VarListPtr& getFirstPrivate() { return firstPrivateClause; }
};

/**
 * OpenMP 'parallel' clause
 */
class OmpParallel: public OmpAnnotation, private OmpCommonClause, private OmpParallelClause {
	OmpReductionPtr		reductionClause;
public:
	OmpParallel(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const OmpDefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const OmpReductionPtr& reductionClause) :
			OmpCommonClause(privateClause, firstPrivateClause),
			OmpParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause), reductionClause(reductionClause) { }

	bool hasReduction() { return static_cast<bool>(reductionClause); }
	const OmpReductionPtr& getReduction() { return reductionClause; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'for' clause
 */
class OmpFor: public OmpAnnotation, private OmpCommonClause, private OmpForClause {
	OmpReductionPtr		reductionClause;
public:
	OmpFor(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& lastPrivateClause,
		const OmpReductionPtr& reductionClause,
		const OmpSchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr,
		bool noWait) :
			OmpCommonClause(privateClause, firstPrivateClause),
			OmpForClause(lastPrivateClause, scheduleClause, collapseExpr, noWait), reductionClause(reductionClause) { }

	bool hasReduction() { return static_cast<bool>(reductionClause); }
	const OmpReductionPtr& getReduction() { return reductionClause; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'parallel for' clause
 */
class OmpParallelFor: public OmpAnnotation, private OmpCommonClause, private OmpParallelClause, private OmpForClause {
	OmpReductionPtr		reductionClause;
public:
	OmpParallelFor(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const OmpDefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const OmpReductionPtr& reductionClause,
		const VarListPtr& lastPrivateClause,
		const OmpSchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr, bool noWait) :
			OmpCommonClause(privateClause, firstPrivateClause),
			OmpParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause),
			OmpForClause(lastPrivateClause, scheduleClause, collapseExpr, noWait), reductionClause(reductionClause) { }

	bool hasReduction() { return static_cast<bool>(reductionClause); }
	const OmpReductionPtr& getReduction() { return reductionClause; }

	void toXml(insieme::xml::XmlElement& elem);
};

class OmpSectionClause {
	VarListPtr			lastPrivateClause;
	OmpReductionPtr		reductionClause;
	bool 				noWait;

public:
	OmpSectionClause(const VarListPtr& lastPrivateClause,
		const OmpReductionPtr& reductionClause,
		bool noWait) : lastPrivateClause(lastPrivateClause), reductionClause(reductionClause), noWait(noWait) { }

	bool hasLastPrivate() { return static_cast<bool>(lastPrivateClause); }
	const VarListPtr& getLastPrivate() { return lastPrivateClause; }

	bool hasReduction() { return static_cast<bool>(reductionClause); }
	const OmpReductionPtr& getReduction() { return reductionClause; }

	bool hasNoWait() { return noWait; }
};

/**
 * OpenMP 'sections' clause
 */
class OmpSections: public OmpAnnotation, private OmpCommonClause, private OmpSectionClause {

public:
	OmpSections(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& lastPrivateClause,
		const OmpReductionPtr& reductionClause,
		bool noWait) :
			OmpCommonClause(privateClause, firstPrivateClause),
			OmpSectionClause(lastPrivateClause, reductionClause, noWait) { }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'parallel sections' clause
 */
class OmpParallelSections: public OmpAnnotation, private OmpCommonClause, private OmpParallelClause, private OmpSectionClause {
public:
	OmpParallelSections(const core::ExpressionPtr& ifClause,
		const core::ExpressionPtr& numThreadClause,
		const OmpDefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& copyinClause,
		const OmpReductionPtr& reductionClause,
		const VarListPtr& lastPrivateClause,
		bool noWait) :
			OmpCommonClause(privateClause, firstPrivateClause),
			OmpParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause),
			OmpSectionClause(lastPrivateClause, reductionClause, noWait) { }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'section' clause
 */
class OmpSection: public OmpAnnotation {
public:
	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'single' clause
 */
class OmpSingle: public OmpAnnotation, private OmpCommonClause {
	VarListPtr			copyPrivateClause;
	bool 				noWait;
public:
	OmpSingle(const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& copyPrivateClause,
		bool noWait) :
			OmpCommonClause(privateClause, firstPrivateClause),
			copyPrivateClause(copyPrivateClause), noWait(noWait) { }

	bool hasCopyPrivate() { return static_cast<bool>(copyPrivateClause); }
	const VarListPtr& getCopyPrivate() { return copyPrivateClause; }

	bool hasNoWait() { return noWait; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'task' clause
 */
class OmpTask: public OmpAnnotation, private OmpCommonClause, private SharedParallelAndTaskClause {
	bool 				untied;
public:
	OmpTask(const core::ExpressionPtr& ifClause,
		bool untied,
		const OmpDefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause) :
			OmpCommonClause(privateClause, firstPrivateClause),
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause), untied(untied) { }

	bool hasUntied() { return untied; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'taskwait' clause
 */
class OmpTaskWait: public OmpAnnotation {
public:
	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'atomic' clause
 */
class OmpAtomic: public OmpAnnotation {
public:
	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'critical' clause
 */
class OmpCritical: public OmpAnnotation {
	core::VariablePtr name;

public:
	OmpCritical(const core::VariablePtr& name): name(name) { }

	bool hasName() { return static_cast<bool>(name); }
	const core::VariablePtr& getName() { return name; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'ordered' clause
 */
class OmpOrdered: public OmpAnnotation {
public:
	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'flush' clause
 */
class OmpFlush: public OmpAnnotation {
	VarListPtr varList;
public:
	OmpFlush(const VarListPtr& varList): varList(varList) { }

	bool hasVarList() { return static_cast<bool>(varList); }
	const VarListPtr& getVarList() { return varList; }

	void toXml(insieme::xml::XmlElement& elem);
};

/**
 * OpenMP 'threadprivate' clause
 */
class OmpThreadPrivate: public OmpAnnotation {
	VarListPtr threadPrivateClause;
public:
	OmpThreadPrivate(const VarListPtr& threadPrivateClause) : threadPrivateClause(threadPrivateClause) { }

	bool hasThreadPrivate() { return static_cast<bool>(threadPrivateClause); }
	const VarListPtr& getThreadPrivate() { return threadPrivateClause; }

	void toXml(insieme::xml::XmlElement& elem);
};

} // End annotation namespace
} // End omp namespace
} // End frontend namespace
} // End insieme namespace
