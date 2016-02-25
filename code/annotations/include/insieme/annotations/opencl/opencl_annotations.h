/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
		virtual std::ostream& printTo(std::ostream & out) const {
			return out;
		}
		virtual void replaceUsage(const core::NodeMap& map) {
			// default annotation references no IR nodes
		}
	};
	typedef std::shared_ptr<Annotation> AnnotationPtr;
	DEFINE_TYPE(BaseAnnotation);
	DEFINE_TYPE(Device);
	DEFINE_TYPE(DeviceAnnotation);
	DEFINE_TYPE(LoopAnnotation);
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
		virtual std::ostream& printTo(std::ostream& out) const;

		virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const;
		void replaceUsage(const core::ExpressionPtr& old, const core::ExpressionPtr& replacement);
		void replaceUsage(const core::NodeMap& map);
	};
	
	class Device : public utils::VirtualPrintable {
	public:
		enum Type { CPU, GPU, ACCELERATOR, DEFAULT = CPU };

		Device(Type type);
		Type getType() const;
		
		virtual std::ostream& printTo(std::ostream & out) const;
		virtual void replaceUsage(const core::NodeMap& map);

		static std::string toString(Type type);
	private:
		const Type type;
	};
	
	class DeviceAnnotation : public Annotation {
		DevicePtr device;
	public:
		DeviceAnnotation(const DevicePtr& device);
		DevicePtr getDevice() const;
		
		virtual std::ostream& printTo(std::ostream & out) const;
		virtual void replaceUsage(const core::NodeMap& map); 
	};
	
	class LoopAnnotation : public Annotation {
		bool independent;
	public:
		LoopAnnotation(bool independent);
		bool getIndependent() const;
		
		virtual std::ostream& printTo(std::ostream& out) const;
		virtual void replaceUsage(const core::NodeMap& map);
	};
	
	class VariableRequirement : public Annotation {
	public:
		enum AccessMode { RO = 0, WO = 1, RW = 2 };
	
		VariableRequirement(const core::VariablePtr& var, const core::ExpressionPtr& size,
							const core::ExpressionPtr& start, const core::ExpressionPtr& end,
							AccessMode accessMode);
		const core::VariablePtr& getVar() const;
		const core::ExpressionPtr& getSize() const;
		const core::ExpressionPtr& getStart() const;
		const core::ExpressionPtr& getEnd() const;
		AccessMode getAccessMode() const;
		
		std::ostream& printTo(std::ostream& out) const override;
	private:
		core::VariablePtr var;
		core::ExpressionPtr size;
		core::ExpressionPtr start;
		core::ExpressionPtr end;
		AccessMode accessMode;
	};
	typedef std::vector<VariableRequirementPtr> VariableRequirementList;
} // End opencl namespace
} // End annotations namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::annotations::opencl::Annotation& ann);
}
