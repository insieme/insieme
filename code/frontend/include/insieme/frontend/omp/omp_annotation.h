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
#include "insieme/core/transform/node_replacer.h"

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



/**
 * This is the root class for OpenMP annotations, be aware that this is not an IR Annotation (see OmpBaseAnnotation).
 */
class Annotation {
public:
	virtual std::ostream& dump(std::ostream& out) const { return out; }
	virtual void replaceUsage (const core::NodeMap& map){
		// default annotation references no IR nodes
	}
};
typedef std::shared_ptr<Annotation> AnnotationPtr;

DEFINE_TYPE(BaseAnnotation);
//DEFINE_TYPE(Annotation);
DEFINE_TYPE(Reduction);
DEFINE_TYPE(Schedule);
DEFINE_TYPE(Collapse);
DEFINE_TYPE(Default);
DEFINE_TYPE(For);
DEFINE_TYPE(Ordered);
DEFINE_TYPE(Single);
DEFINE_TYPE(Task);
DEFINE_TYPE(TaskWait);
DEFINE_TYPE(Parallel);
DEFINE_TYPE(ParallelFor);
DEFINE_TYPE(Barrier);
DEFINE_TYPE(Critical);
DEFINE_TYPE(Master);
DEFINE_TYPE(Flush);
DEFINE_TYPE(Atomic);

/**
 * OpenMP+ extensions
 */

DEFINE_TYPE(Param);
DEFINE_TYPE(Target);
DEFINE_TYPE(Objective);
DEFINE_TYPE(Region);
DEFINE_TYPE(SharedOMPP);

// OMPA extension
DEFINE_TYPE(Approximate);

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
class BaseAnnotation : public insieme::utils::CompoundAnnotation<omp::Annotation ,core::NodeAnnotation> {
public:
	static const string NAME;
    static const insieme::utils::StringKey<BaseAnnotation> KEY;

    BaseAnnotation(const insieme::utils::CompoundAnnotation< omp::Annotation >::AnnotationList& annotationList):
    	insieme::utils::CompoundAnnotation< omp::Annotation , core::NodeAnnotation >(annotationList) { }

    const insieme::utils::AnnotationKeyPtr getKey() const { return &KEY; }
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


	void replaceUsage (const core::ExpressionPtr& old, const core::ExpressionPtr& replacement){
		core::NodeMap map;
		map [old] = replacement;
		replaceUsage(map);
	}

	void replaceUsage (const core::NodeMap& map){
		// for each annotation in the list
		for (auto cur : getAnnotationList()){
			cur->replaceUsage (map);
		}
	}
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

/** 
 * function to help with the variable replacement inside of stored expressions
 */
void replaceVars (core::ExpressionPtr& expr, core::NodeMap map);

/** 
 * function to help with the variable replacement inside of variable lists
 */
void replaceVars (VarListPtr& list, core::NodeMap map);


class Reduction {
public:
	// operator = + or - or * or & or | or ^ or && or ||
	enum Operator { PLUS, MINUS, MUL, AND, OR, XOR, LAND, LOR };

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
		case MUL: 	return "*";
		case AND: 	return "&";
		case OR:	return "|";
		case XOR:	return "^";
		case LAND:	return "&&";
		case LOR:	return "||";
		}
		assert(false && "Operator doesn't exist");
		return "?";
	}

	void replaceUsage (const core::NodeMap& map){
		if(vars)replaceVars (vars, map);
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
		return "?";
	}

	void replaceUsage (const core::NodeMap& map){
		if(chunkExpr)replaceVars (chunkExpr, map);
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
		return "?";
	}

private:
	Kind mode;
};


class Approximate {
public:
	Approximate(const core::ExpressionPtr& target, const core::ExpressionPtr& replacement) :
		available(true), approxTarget(target), approxReplacement(replacement) { }

	Approximate(const ApproximatePtr& ptr) {
		if(ptr) {
			*this = *ptr;
		} else {
			available = false;
		}
	}

	bool hasApproximate() { return available; }
	core::ExpressionPtr getApproximateTarget() { return approxTarget; }
	core::ExpressionPtr getApproximateReplacement() { return approxReplacement; }

private:
	bool available;
	core::ExpressionPtr approxTarget, approxReplacement;
};

/**
 * Represents the OpenMP+ Param clause that may appears in region.
 * param(var, [range(l,u,s) | enum(A,s)])
 */
class Param {
public:
	Param(const core::ExpressionPtr& var,
			const std::shared_ptr<core::ExpressionList>& range,
			const std::shared_ptr<core::ExpressionList>& quality_range,
			const core::ExpressionPtr& enumList,
			const core::ExpressionPtr& enumSize): var(var), range(range), quality_range(quality_range), enumList(enumList), enumSize(enumSize) {}

	const core::ExpressionPtr& getVar() const { assert(var); return var; }

	bool hasRange() const { return static_cast<bool>(range && range->size() == 3); }
	const core::ExpressionPtr& getRangeLBound() const { assert(hasRange()); return (range->at(0)); }
	const core::ExpressionPtr& getRangeUBound() const { assert(hasRange()); return (range->at(1)); }
	const core::ExpressionPtr& getRangeStep() const { assert(hasRange()); return (range->at(2)); }

	bool hasQualityRange() const { return static_cast<bool>(quality_range && quality_range->size() == 3); }
	const core::ExpressionPtr& getQualityRangeLBound() const { assert(hasQualityRange()); return (quality_range->at(0)); }
	const core::ExpressionPtr& getQualityRangeUBound() const { assert(hasQualityRange()); return (quality_range->at(1)); }
	const core::ExpressionPtr& getQualityRangeStep() const { assert(hasQualityRange()); return (quality_range->at(2)); }

	bool hasEnum() const { return static_cast<bool>(enumList); }
	const core::ExpressionPtr& getEnumList() const { assert(hasEnum()); return enumList; }
	const core::ExpressionPtr& getEnumSize() const { assert(hasEnum()); return enumSize; }

	std::ostream& dump(std::ostream& out) const {
		out << "param(" << *var;
		if(hasRange())
			out << ", range(" << *(range->at(0)) << ", " << *(range->at(1)) << ", " << *(range->at(2));
		if(hasQualityRange())
			out << "; " << *(quality_range->at(0)) << ", " << *(quality_range->at(1)) << ", " << *(quality_range->at(2)) << ")";
        else
            out << ")";
		if(hasEnum())
			out << ", enum(" << *enumList << ", " << *enumSize << ")";
		return out << ")";
	}

	void replaceUsage (const core::NodeMap& map){
		if(var)replaceVars (var, map);
		if(range)replaceVars (range, map);
		if(quality_range)replaceVars (quality_range, map);
		if(enumList)replaceVars (enumList, map);
		if(enumSize)replaceVars (enumSize, map);
	}

private:
	core::ExpressionPtr var;
	std::shared_ptr<core::ExpressionList> range;
	std::shared_ptr<core::ExpressionList> quality_range;
	core::ExpressionPtr enumList;
	core::ExpressionPtr enumSize;
};

/**
 * Represents the OpenMP+ Target clause that may appears in region, task and parallel.
 * target(target-type[:group-id[:core-id]])
 */
class Target {
public:
	enum Type { GENERAL, ACCELERATOR };

	Target(const Type& type,
			const std::shared_ptr<core::ExpressionList>& groupIds,
			const core::ExpressionPtr& groupIdsRangeUpper,
			const std::shared_ptr<core::ExpressionList>& coreIds,
			const core::ExpressionPtr& coreIdsRangeUpper) : type(type), groupIds(groupIds), groupIdsRangeUpper(groupIdsRangeUpper),
					coreIds(coreIds), coreIdsRangeUpper(coreIdsRangeUpper) {}

	bool hasGroupIds() const { return static_cast<bool>(groupIds); }
	const core::ExpressionList& getGroupIds() const { assert(hasGroupIds()); return *(groupIds); }

	bool hasGroupIdsRange() const { return static_cast<bool>(groupIdsRangeUpper); }
	const core::Expression& getGroupIdsRangeUpper() const { assert(hasGroupIdsRange()); return *(groupIdsRangeUpper); }

	bool hasCoreIds() const { return static_cast<bool>(coreIds); }
	const core::ExpressionList& getCoreIds() const { assert(hasCoreIds()); return *(coreIds); }

	bool hasCoreIdsRange() const { return static_cast<bool>(coreIdsRangeUpper); }
	const core::Expression& getCoreIdsRangeUpper() const { assert(hasCoreIdsRange()); return *(coreIdsRangeUpper); }

	static std::string typeToStr(Type t) {
		switch(t) {
		case GENERAL: 		return "general";
		case ACCELERATOR: 	return "accelerator";
		}
		assert(false && "Type doesn't exist");
		return "?";
	}

	std::ostream& dump(std::ostream& out) const {
		out << "target(" << typeToStr(type);
		if(hasGroupIds())
			out << ": " << join(",", *groupIds);
		if(hasGroupIdsRange())
			out << " ... " << *groupIdsRangeUpper;
		if(hasCoreIds())
			out << ": " << join(",", *coreIds);
		if(hasCoreIdsRange())
			out << " ... " << *coreIdsRangeUpper;
		return out << ")";
	}

	void replaceUsage (const core::NodeMap& map){
		if(groupIds)replaceVars (groupIds, map);
		if(groupIdsRangeUpper)replaceVars (groupIdsRangeUpper, map);
		if(coreIds)replaceVars (coreIds, map);
		if(coreIdsRangeUpper)replaceVars (coreIdsRangeUpper, map);
	}

private:
	Type type;
	std::shared_ptr<core::ExpressionList> groupIds;
	core::ExpressionPtr groupIdsRangeUpper;
	std::shared_ptr<core::ExpressionList> coreIds;
	core::ExpressionPtr coreIdsRangeUpper;
};

/**
 * Represents the OpenMP+ Objective clause that may appears in region, task and parallel.
 * objective(weights, constraints)
 */
class Objective {
public:
	// operator = < or <= or == or >= or >
	enum Operator { LESS, LESSEQUAL, EQUALEQUAL, GREATEREQUAL, GREATER };
	// parameter = T or E or P
	enum Parameter { TIME, ENERGY, POWER, QUALITY};

	typedef std::vector<Operator> OperatorList;
	typedef std::vector<Parameter> ParameterList;

	Objective(const double timeWeight,
				const double energyWeight,
				const double powerWeight,
				const double qualityWeight,
				const std::shared_ptr<ParameterList>& constraintsParams,
				const std::shared_ptr<OperatorList>& constraintsOps,
				const std::shared_ptr<core::ExpressionList>& constraintsExprs) : timeWeight(timeWeight), energyWeight(energyWeight),
						powerWeight(powerWeight), qualityWeight(qualityWeight), constraintsParams(constraintsParams), constraintsOps(constraintsOps), constraintsExprs(constraintsExprs) {}

	bool hasTimeWeight() const { return true; }
	const double getTimeWeight() const { assert(hasTimeWeight()); return timeWeight; }

	bool hasEnergyWeight() const { return true; }
	const double getEnergyWeight() const { assert(hasEnergyWeight()); return energyWeight; }

	bool hasPowerWeight() const { return true; }
	const double getPowerWeight() const { assert(hasPowerWeight()); return powerWeight; }

	bool hasQualityWeight() const { return true; }
	const double getQualityWeight() const { assert(hasQualityWeight()); return qualityWeight; }

	bool hasConstraintsParams() const { return static_cast<bool>(constraintsParams); }
	const std::vector<Parameter>& getConstraintsParams() const { assert(hasConstraintsParams()); return *constraintsParams; }

	bool hasConstraintsOps() const { return static_cast<bool>(constraintsOps); }
	const std::vector<Operator>& getConstraintsOps() const { assert(hasConstraintsOps()); return *constraintsOps; }

	bool hasConstraintsExprs() const { return static_cast<bool>(constraintsExprs); }
	const core::ExpressionList& getConstraintsExprs() const { assert(hasConstraintsExprs()); return *constraintsExprs; }

	static std::string opToStr(Operator op) {
		switch(op) {
		case LESS: 			return "<";
		case LESSEQUAL: 	return "<=";
		case EQUALEQUAL: 	return "==";
		case GREATEREQUAL: 	return ">=";
		case GREATER:	 	return ">";
		}
		assert(false && "Operator doesn't exist");
		return "?";
	}

	static std::string paramToStr(Parameter param) {
		switch(param) {
		case TIME: 		return "T";
		case ENERGY: 	return "E";
		case POWER: 	return "P";
		case QUALITY: 	return "Q";
		}
		assert(false && "Parameter doesn't exist");
		return "?";
	}

	std::ostream& dump(std::ostream& out) const {
		out << "objective(";
		if(hasEnergyWeight())
			out << "E * " << energyWeight << " + ";
		else
			out << "E * 0 + ";
		if(hasPowerWeight())
			out << "P * " << powerWeight << " + ";
		else
			out << "P * 0 + ";
		if(hasQualityWeight())
			out << "Q * " << qualityWeight << " + ";
		else
			out << "Q * 0 + ";
		if(hasTimeWeight())
			out << "T * " << timeWeight << ": ";
		else
			out << "T * 0: ";
		if(hasConstraintsParams() && hasConstraintsOps() && hasConstraintsExprs()
				&& constraintsParams->size() > 0
				&& constraintsParams->size() == constraintsOps->size()
				&& constraintsParams->size() == constraintsExprs->size())
		{
			out << paramToStr(constraintsParams->front()) << " " << opToStr(constraintsOps->front()) << *(constraintsExprs->front());

			for(size_t pos=1; pos<constraintsParams->size(); ++pos)
			{
				out << "; " << paramToStr(constraintsParams->at(pos)) << " " << opToStr(constraintsOps->at(pos)) << *(constraintsExprs->at(pos));
			}
		}
		return out << ")";
	}

	void replaceUsage (const core::NodeMap& map){
		if(constraintsExprs)replaceVars (constraintsExprs, map);
	}

private:
	double timeWeight;
	double energyWeight;
	double powerWeight;
	double qualityWeight;
	std::shared_ptr<std::vector<Parameter>> constraintsParams;
	std::shared_ptr<std::vector<Operator>> constraintsOps;
	std::shared_ptr<core::ExpressionList> constraintsExprs;
};

/**
 * OpenMP 'master' clause
 */
class Master: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "master"; }
};

class SharedOMPP {
protected:
	TargetPtr		targetClause;
	ObjectivePtr	objectiveClause;
	ParamPtr        paramClause;

public:
	SharedOMPP(const TargetPtr& targetClause, const ObjectivePtr& objectiveClause, const ParamPtr& paramClause): targetClause(targetClause), objectiveClause(objectiveClause), paramClause(paramClause) { }

	bool hasTarget() const { return static_cast<bool>(targetClause); }
	const Target& getTarget() const { assert(hasTarget()); return *targetClause; }

	bool hasObjective() const { return static_cast<bool>(objectiveClause); }
	const Objective& getObjective() const { assert(hasObjective()); return *objectiveClause; }

	bool hasParam() const { return static_cast<bool>(paramClause); }
	const Param& getParam() const { assert(hasParam()); return *paramClause; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		if(targetClause) targetClause->replaceUsage(map);
		if(objectiveClause) objectiveClause->replaceUsage(map);
		if(paramClause) paramClause->replaceUsage(map);
	}
};

/**
 * Represent clauses which are
 */
class ForClause : public virtual SharedOMPP {
protected:
	VarListPtr			lastPrivateClause;
	VarListPtr			lastLocalClause;
	SchedulePtr			scheduleClause;
	core::ExpressionPtr	collapseExpr;
	bool 				noWait;
	bool 				ordered;
public:
	ForClause( const VarListPtr& lastPrivateClause, const VarListPtr& lastLocalClause, const SchedulePtr& scheduleClause, const core::ExpressionPtr& collapseExpr, 
			const TargetPtr& targetClause, const ObjectivePtr& objectiveClause, const ParamPtr& paramClause, bool noWait, bool ordered) :
        SharedOMPP(targetClause, objectiveClause, paramClause), lastPrivateClause(lastPrivateClause), 
		lastLocalClause(lastLocalClause), scheduleClause(scheduleClause), collapseExpr(collapseExpr), noWait(noWait), ordered(ordered) {}

	bool hasLastPrivate() const { return static_cast<bool>(lastPrivateClause); }
	const VarList& getLastPrivate() const { assert(hasLastPrivate()); return *lastPrivateClause; }

	bool hasLastLocal() const { return static_cast<bool>(lastLocalClause); }
	const VarList& getLastLocal() const { assert(hasLastLocal()); return *lastLocalClause; }

	bool hasSchedule() const { return static_cast<bool>(scheduleClause); }
	const Schedule& getSchedule() const { assert(hasSchedule()); return *scheduleClause; }

	bool hasCollapse() const { return static_cast<bool>(collapseExpr); }
	const core::Expression& getCollapse() const { assert(hasCollapse()); return *collapseExpr; }

	bool hasNoWait() const { return noWait; }
	bool hasOrdered() const { return ordered; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		if(lastPrivateClause) replaceVars (lastPrivateClause, map);
		if(lastLocalClause) replaceVars (lastLocalClause, map);
		if(collapseExpr)	 replaceVars (collapseExpr, map);
		if(scheduleClause) 	scheduleClause->replaceUsage(map);
		SharedOMPP::replaceUsage(map);
	}
};

class SharedParallelAndTaskClause : public virtual SharedOMPP {
protected:
	core::ExpressionPtr	ifClause;
	DefaultPtr			defaultClause;
	VarListPtr			sharedClause;
public:
	SharedParallelAndTaskClause(const core::ExpressionPtr& ifClause, const DefaultPtr& defaultClause, const VarListPtr& sharedClause,
			const TargetPtr& targetClause, const ObjectivePtr& objectiveClause, const ParamPtr& paramClause) :
		SharedOMPP(targetClause, objectiveClause, paramClause),
		ifClause(ifClause), defaultClause(defaultClause), sharedClause(sharedClause) { }

	bool hasIf() const { return static_cast<bool>(ifClause); }
	const core::ExpressionPtr& getIf() const { assert(hasIf()); return ifClause; }

	bool hasDefault() const { return static_cast<bool>(defaultClause); }
	const Default& getDefault() const { assert(hasDefault()); return *defaultClause; }

	bool hasShared() const { return static_cast<bool>(sharedClause); }
	const VarList& getShared() const { assert(hasShared()); return *sharedClause; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		replaceVars (ifClause, map);
		replaceVars (sharedClause, map);
		SharedOMPP::replaceUsage(map);
	}
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
		const VarListPtr& copyinClause,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause,
        const ParamPtr& paramClause):
            SharedOMPP(targetClause, objectiveClause, paramClause),
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause, targetClause, objectiveClause, paramClause),
			numThreadClause(numThreadClause), copyinClause(copyinClause) { }

	bool hasNumThreads() const { return static_cast<bool>(numThreadClause); }
	const core::ExpressionPtr& getNumThreads() const { assert(hasNumThreads()); return numThreadClause; }

	bool hasCopyin() const { return static_cast<bool>(copyinClause); }
	const VarList& getCopyin() const { assert(hasCopyin()); return *copyinClause; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		replaceVars (copyinClause, map);
		replaceVars (numThreadClause, map);
		SharedParallelAndTaskClause::replaceUsage(map);
	}
};

class CommonClause {
protected:
	VarListPtr	privateClause;
	VarListPtr	firstPrivateClause;
	VarListPtr	localClause;
	VarListPtr	firstLocalClause;
public:
	CommonClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause,
			const VarListPtr& localClause = VarListPtr(), const VarListPtr& firstLocalClause = VarListPtr()):
			privateClause(privateClause), firstPrivateClause(firstPrivateClause),
			localClause(localClause), firstLocalClause(firstLocalClause) { }

	bool hasPrivate() const { return static_cast<bool>(privateClause); }
	const VarList& getPrivate() const { assert(hasPrivate()); return *privateClause; }

	bool hasFirstPrivate() const { return static_cast<bool>(firstPrivateClause) && firstPrivateClause->size() > 0; }
	const VarList& getFirstPrivate() const { assert(hasFirstPrivate()); return *firstPrivateClause; }

	bool hasLocal() const { return static_cast<bool>(localClause); }
	const VarList& getLocal() const { assert(hasLocal()); return *localClause; }

	bool hasFirstLocal() const { return static_cast<bool>(firstLocalClause); }
	const VarList& getFirstLocal() const { assert(hasFirstLocal()); return *firstLocalClause; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		if(privateClause)  		replaceVars (privateClause, map);
		if(firstPrivateClause) 	replaceVars (firstPrivateClause, map);
		if(localClause) 		replaceVars (localClause, map);
		if(firstLocalClause) 	replaceVars (firstLocalClause, map);
	}
};


/**
 * Interface to enable common access to data sharing clauses
 */
class DatasharingClause : public CommonClause {
public:
	DatasharingClause(const VarListPtr& privateClause, const VarListPtr& firstPrivateClause,
			const VarListPtr& localClause = VarListPtr(), const VarListPtr& firstLocalClause = VarListPtr()):
			CommonClause(privateClause, firstPrivateClause, localClause, firstLocalClause) { }

	virtual bool hasReduction() const = 0;
	virtual const Reduction& getReduction() const = 0;
	
	virtual void replaceUsage (const core::NodeMap& map){
		CommonClause::replaceUsage(map);
	}
};

/**
 * OpenMP+ 'region' clause
 */
class Region: public DatasharingClause, public Annotation, public virtual SharedOMPP {
protected:
	Reduction dummy;
public:
	Region(const ParamPtr& paramClause,
			const VarListPtr& localClause,
			const VarListPtr& firstLocalClause,
			const TargetPtr& targetClause,
			const ObjectivePtr& objectiveClause) :
				SharedOMPP(targetClause, objectiveClause, paramClause),
				DatasharingClause(VarListPtr(), VarListPtr(), localClause, firstLocalClause),
				dummy(Reduction::PLUS, VarListPtr()) {}

	virtual bool hasReduction() const { return false; }
	virtual const Reduction& getReduction() const { assert(false); return dummy; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		DatasharingClause::replaceUsage(map);
		Annotation::replaceUsage(map);
		SharedOMPP::replaceUsage(map);
	}
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
		const ReductionPtr& reductionClause,
		const VarListPtr& localClause,
		const VarListPtr& firstLocalClause,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause,
        const ParamPtr& paramClause) :
            SharedOMPP(targetClause, objectiveClause, paramClause),
			DatasharingClause(privateClause, firstPrivateClause, localClause, firstLocalClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause, targetClause, objectiveClause, paramClause),
			reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	std::ostream& dump(std::ostream& out) const;
	
	virtual void replaceUsage (const core::NodeMap& map){
		DatasharingClause::replaceUsage(map);
		Annotation::replaceUsage(map);
		ParallelClause::replaceUsage(map);
		if (hasReduction()) reductionClause->replaceUsage(map);
	}
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
		const VarListPtr& localClause,
		const VarListPtr& firstLocalClause,
		const VarListPtr& lastLocalClause,
		const ReductionPtr& reductionClause,
		const SchedulePtr& scheduleClause,
		const core::ExpressionPtr& collapseExpr,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause, 
        const ParamPtr& paramClause, bool noWait, bool ordered) :
            SharedOMPP(targetClause, objectiveClause, paramClause),
			DatasharingClause(privateClause, firstPrivateClause, localClause, firstLocalClause),
			ForClause(lastPrivateClause, lastLocalClause, scheduleClause, collapseExpr, targetClause, objectiveClause, paramClause, noWait, ordered), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		DatasharingClause::replaceUsage(map);
		Annotation::replaceUsage(map);
		ForClause::replaceUsage(map);
		if (hasReduction()) reductionClause->replaceUsage (map);
	}
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
		const core::ExpressionPtr& collapseExpr,
		const VarListPtr& localClause,
		const VarListPtr& firstLocalClause,
		const VarListPtr& lastLocalClause,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause, 
        const ParamPtr& paramClause, bool noWait, bool ordered) :
            SharedOMPP(targetClause, objectiveClause, paramClause),
			CommonClause(privateClause, firstPrivateClause, localClause, firstLocalClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause, targetClause, objectiveClause, paramClause),
			ForClause(lastPrivateClause, lastLocalClause, scheduleClause, collapseExpr, targetClause, objectiveClause, paramClause, noWait, ordered), reductionClause(reductionClause) { }

	bool hasReduction() const { return static_cast<bool>(reductionClause); }
	const Reduction& getReduction() const { assert(hasReduction()); return *reductionClause; }

	ParallelPtr toParallel() const {
		return std::make_shared<Parallel>(ifClause, numThreadClause, defaultClause, privateClause, 
			firstPrivateClause, sharedClause, copyinClause, reductionClause, localClause, firstLocalClause, targetClause, objectiveClause, paramClause);
	}
	ForPtr toFor() const {
		// do not duplicate stuff already handled in parallel
		return std::make_shared<For>(/*private*/VarListPtr(), /*firstprivate*/VarListPtr(), lastPrivateClause, /*local*/VarListPtr(), /*firstlocal*/VarListPtr(), lastLocalClause,
			/*reduction*/ReductionPtr(), scheduleClause, collapseExpr, targetClause, objectiveClause, paramClause, noWait, ordered);
	}

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage(map);
		CommonClause::replaceUsage(map);
		ParallelClause::replaceUsage(map);
		ForClause::replaceUsage(map);
		if (hasReduction()) reductionClause->replaceUsage (map);
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

	std::ostream& dump(std::ostream& out) const;
	
	virtual void replaceUsage (const core::NodeMap& map){
		replaceVars (lastPrivateClause, map);
		if (hasReduction()) reductionClause->replaceUsage (map);
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

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		CommonClause::replaceUsage(map);
		SectionClause::replaceUsage(map);
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
		const VarListPtr& localClause,
		const VarListPtr& firstLocalClause,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause,
		const ParamPtr& paramClause,
		bool noWait) :
            SharedOMPP(targetClause, objectiveClause, paramClause),
			CommonClause(privateClause, firstPrivateClause, localClause, firstLocalClause),
			ParallelClause(ifClause, numThreadClause, defaultClause, sharedClause, copyinClause,
					targetClause, objectiveClause, paramClause),
			SectionClause(lastPrivateClause, reductionClause, noWait) { }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
		CommonClause::replaceUsage ( map);
		ParallelClause::replaceUsage ( map);
		SectionClause::replaceUsage ( map);
	}
};

/**
 * OpenMP 'section' clause
 */
class Section: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "section"; }

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
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
	
	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
		replaceVars (copyPrivateClause, map);
		CommonClause::replaceUsage(map);
	}
};

/**
 * OpenMP 'task' clause
 */
class Task: public DatasharingClause, public Annotation, public SharedParallelAndTaskClause, public Approximate {
	bool 	untied;
	Reduction dummy;
public:
	Task(const core::ExpressionPtr& ifClause,
		bool untied,
		const DefaultPtr& defaultClause,
		const VarListPtr& privateClause,
		const VarListPtr& firstPrivateClause,
		const VarListPtr& sharedClause,
		const VarListPtr& localClause,
		const VarListPtr& firstLocalClause,
		const TargetPtr& targetClause,
		const ObjectivePtr& objectiveClause,
        const ParamPtr& paramClause,
		const ApproximatePtr& approximateClause) :
            SharedOMPP(targetClause, objectiveClause, paramClause),
			DatasharingClause(privateClause, firstPrivateClause, localClause, firstLocalClause),
			SharedParallelAndTaskClause(ifClause, defaultClause, sharedClause, targetClause, objectiveClause, paramClause),
			Approximate(approximateClause),
			untied(untied), dummy(Reduction::PLUS, VarListPtr()) { }

	bool hasUntied() const { return untied; }
	
	bool hasReduction() const { return false; }
	const Reduction& getReduction() const { assert(false); return dummy; }

	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		DatasharingClause::replaceUsage(map);
		Annotation::replaceUsage ( map);
		SharedParallelAndTaskClause::replaceUsage(map);
		if(hasReduction()) dummy.replaceUsage(map);
	}
};

/**
 * OpenMP 'taskwait' clause
 */
class TaskWait: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "task wait"; }

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
};

/**
 * OpenMP 'atomic' clause
 */
class Atomic: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "atomic"; }

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
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

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
};

/**
 * OpenMP 'ordered' clause
 */
class Ordered: public Annotation {
public:
	std::ostream& dump(std::ostream& out) const { return out << "ordered"; }

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
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
	
	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
		replaceVars (varList, map);
	}
};

/**
 * OpenMP 'threadprivate' clause
 */
struct ThreadPrivate: public Annotation {
	std::ostream& dump(std::ostream& out) const;

	virtual void replaceUsage (const core::NodeMap& map){
		Annotation::replaceUsage ( map);
	}
};

} // End omp namespace
} // End frontend namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::frontend::omp::Annotation& ann);
}
