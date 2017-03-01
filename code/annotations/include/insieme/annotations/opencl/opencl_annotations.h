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
#pragma once

#include "insieme/utils/annotation.h"
#include "insieme/utils/string_utils.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/transform/node_replacer.h"
#include <memory.h>

#define DEFINE_TYPE(Type) \
	class Type; \
	typedef std::shared_ptr<Type> Type##Ptr;

namespace insieme {

namespace annotations {
namespace opencl {
	/**
	 * This is the root class for OpenCL annotations, be aware that this is not an IR Annotation (see BaseAnnotation).
	 */
	class Annotation : public utils::VirtualPrintable {
	  public:
		virtual ~Annotation() { }
		virtual void replaceVars(const core::VarExprMap& map) {}
		virtual std::ostream& printTo(std::ostream& out) const { return out; }
	};
	typedef std::shared_ptr<Annotation> AnnotationPtr;
	DEFINE_TYPE(BaseAnnotation);
	DEFINE_TYPE(Device);
	DEFINE_TYPE(DeviceAnnotation);
	DEFINE_TYPE(LoopAnnotation);
	DEFINE_TYPE(VariableRange);
	DEFINE_TYPE(VariableRequirement);

	/**
	 * It implements the annotation node which is attached to the insieme IR for OpenCL directives
	 */
	class BaseAnnotation : public insieme::utils::CompoundAnnotation<opencl::Annotation, core::NodeAnnotation> {
	public:
		static const string NAME;
		static const insieme::utils::StringKey<BaseAnnotation> KEY;

		BaseAnnotation(const insieme::utils::CompoundAnnotation<opencl::Annotation>::AnnotationList& annotationList);
		const insieme::utils::AnnotationKeyPtr getKey() const;
		const std::string& getAnnotationName() const;

		const std::string toString() const;
		virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const;

		void replaceVars(const core::VarExprMap& map);
		std::ostream& printTo(std::ostream& out) const;
	};
	
	class Device : public Annotation {
	public:
		enum Type { CPU, GPU, ACCELERATOR, ALL = (unsigned) -1 };

		Device(Type type);
		Type getType() const;
		
		static std::string toString(Type type);

		void replaceVars(const core::VarExprMap& map) override;
		std::ostream& printTo(std::ostream& out) const override;
	private:
		const Type type;
	};
	
	class DeviceAnnotation : public Annotation {
		DevicePtr device;
	public:
		DeviceAnnotation(const DevicePtr& device);
		DevicePtr getDevice() const;

		void replaceVars(const core::VarExprMap& map) override;
		std::ostream& printTo(std::ostream& out) const override;
	};
	
	class LoopAnnotation : public Annotation {
		bool independent;
	public:
		LoopAnnotation(bool independent);
		bool getIndependent() const;

		void replaceVars(const core::VarExprMap& map) override;
		std::ostream& printTo(std::ostream& out) const override;
	};
	
	class VariableRange : public Annotation {
	public:
		VariableRange(const core::ExpressionPtr& size, const core::ExpressionPtr& start,
					  const core::ExpressionPtr& end);
		const core::ExpressionPtr& getSize() const;
		const core::ExpressionPtr& getStart() const;
		const core::ExpressionPtr& getEnd() const;

		void replaceVars(const core::VarExprMap& map) override;
		std::ostream& printTo(std::ostream& out) const override;
	private:
		core::ExpressionPtr size;
		core::ExpressionPtr start;
		core::ExpressionPtr end;
	};
	typedef std::vector<VariableRangePtr> VariableRangeList;

	class VariableRequirement : public Annotation {
	public:
		enum AccessMode { RO = 0, WO = 1, RW = 2 };

		VariableRequirement(const core::VariablePtr& var, AccessMode accessMode, const VariableRangeList& ranges);
		const core::VariablePtr& getVar() const;
		AccessMode getAccessMode() const;
		const VariableRangeList& getRanges() const;

		void replaceVars(const core::VarExprMap& map) override;
		std::ostream& printTo(std::ostream& out) const override;
	private:
		core::VariablePtr var;
		AccessMode accessMode;
		VariableRangeList ranges;
	};
	typedef std::vector<VariableRequirementPtr> VariableRequirementList;
} // End opencl namespace
} // End annotations namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::annotations::opencl::Annotation& ann);
}
