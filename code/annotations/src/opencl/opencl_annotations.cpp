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
#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/ir_builder.h"

#include <memory>

namespace insieme {
namespace annotations {
namespace opencl {

	const string BaseAnnotation::NAME = "OpenCLAnnotation";
	const utils::StringKey<BaseAnnotation> BaseAnnotation::KEY("OpenCL");

	BaseAnnotation::BaseAnnotation(const insieme::utils::CompoundAnnotation<opencl::Annotation>::AnnotationList& annotationList) :
		insieme::utils::CompoundAnnotation<opencl::Annotation, core::NodeAnnotation>(annotationList)
	{ }

	const insieme::utils::AnnotationKeyPtr BaseAnnotation::getKey() const {
		return &KEY;
	}
	
	const std::string& BaseAnnotation::getAnnotationName() const {
		return NAME;
	}
	
	const std::string BaseAnnotation::toString() const {
		std::ostringstream ss;
		for(AnnotationList::const_iterator it = getAnnotationListBegin(), end = getAnnotationListEnd(); it != end; ++it) {
			(*it)->printTo(ss);
			if(it + 1 != end) { ss << "\\n"; }
		}
		return ss.str();
	};

	std::ostream& BaseAnnotation::printTo(std::ostream& out) const {
		return out << NAME << toString();
	}

	bool BaseAnnotation::migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
		// always copy the annotation
		assert(&*ptr == this && "Annotation pointer should reference this annotation!");
		after->addAnnotation(ptr);
		return true;
	}

	void BaseAnnotation::replaceVars(const core::VarExprMap& map) {
		for(auto cur : getAnnotationList()) cur->replaceVars(map);
	}

	Device::Device(Type type) :
		type(type)
	{ }
	
	Device::Type Device::getType() const {
		return type;
	}
	
	std::ostream& Device::printTo(std::ostream& out) const {
		out << toString(type);
		return out;
	}
	
	void Device::replaceVars(const core::VarExprMap& map) { }
	
	std::string Device::toString(Type type) {
		switch(type) {
		case CPU:			return "CPU";
		case GPU:			return "GPU";
		case ACCELERATOR:	return "ACCELERATOR";
		case ALL:			return "ALL";
		}
		assert_fail() << "given type " << type << " does not exist!";
		return "?";
	}
	
	DeviceAnnotation::DeviceAnnotation(const DevicePtr& device) :
		device(device)
	{ }
	
	DevicePtr DeviceAnnotation::getDevice() const {
		return device;
	}
		
	std::ostream& DeviceAnnotation::printTo(std::ostream & out) const {
		return device->printTo(out);
	}
	
	void DeviceAnnotation::replaceVars(const core::VarExprMap& map) {
		device->replaceVars(map);
	}

	LoopAnnotation::LoopAnnotation(bool independent) :
		independent(independent)
	{ }
	
	bool LoopAnnotation::getIndependent() const {
		return independent;
	}
		
	std::ostream& LoopAnnotation::printTo(std::ostream& out) const {
		out << "loop";
		if(independent) out << " independent";
		return out;
	}
	
	void LoopAnnotation::replaceVars(const core::VarExprMap& map) { }
	
	VariableRange::VariableRange(const core::ExpressionPtr& size, const core::ExpressionPtr& start,
								 const core::ExpressionPtr& end) :
		size(size), start(start), end(end)
	{ }

	const core::ExpressionPtr& VariableRange::getSize() const {
		return size;
	}

	const core::ExpressionPtr& VariableRange::getStart() const {
		return start;
	}

	const core::ExpressionPtr& VariableRange::getEnd() const {
		return end;
	}

	void VariableRange::replaceVars(const core::VarExprMap& map) {
		if (map.empty()) return;

		auto& manager = size->getNodeManager();
		size  = core::transform::replaceVarsGen(manager, size, map);
		start = core::transform::replaceVarsGen(manager, start, map);
		end   = core::transform::replaceVarsGen(manager, end, map);
	}

	std::ostream& VariableRange::printTo(std::ostream& out) const {
		return out << "{" << toString(*size) << "," << toString(*start) << "," << toString(*end) << "}";
	}

	VariableRequirement::VariableRequirement(const core::VariablePtr& var, AccessMode accessMode, const VariableRangeList& ranges) :
		var(var), accessMode(accessMode), ranges(ranges)
	{ }

	const core::VariablePtr& VariableRequirement::getVar() const {
		return var;
	}

	VariableRequirement::AccessMode VariableRequirement::getAccessMode() const {
		return accessMode;
	}

	const VariableRangeList& VariableRequirement::getRanges() const {
		return ranges;
	}

	void VariableRequirement::replaceVars(const core::VarExprMap& map) {
		if (map.empty()) return;

		var = core::transform::replaceVarsGen(var->getNodeManager(), var, map);
		for (auto range : ranges) range->replaceVars(map);
	}

	std::ostream& VariableRequirement::printTo(std::ostream& out) const {
		out << "{" << toString(*var) << "," << static_cast<int>(accessMode) << ",{";
		for (const auto& range: ranges) range->printTo(out);
		out << "}}";
		return out;
	}
} // End opencl namespace
} // End annotations namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::annotations::opencl::Annotation& ann) {
		return ann.printTo(os);
	}
}
