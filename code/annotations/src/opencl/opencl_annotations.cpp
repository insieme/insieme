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

#include "insieme/annotations/opencl/opencl_annotations.h"
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

	void BaseAnnotation::replaceUsage(const core::ExpressionPtr& old, const core::ExpressionPtr& replacement) {
		core::NodeMap map;
		map[old] = replacement;
		replaceUsage(map);
	}

	void BaseAnnotation::replaceUsage(const core::NodeMap& map) {
		// for each annotation in the list
		for(auto cur : getAnnotationList()) {
			cur->replaceUsage(map);
		}
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
	
	void Device::replaceUsage(const core::NodeMap& map) {
		// if(vars) { replaceVars(vars, map); }
	}
	
	std::string Device::toString(Type type) {
		switch(type) {
		case CPU:			return "CPU";
		case GPU:			return "GPU";
		case ACCELERATOR:	return "ACCELERATOR";
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
	
	void DeviceAnnotation::replaceUsage(const core::NodeMap& map) {
		device->replaceUsage(map);
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
	
	void LoopAnnotation::replaceUsage(const core::NodeMap& map) {
		return;
	}
} // End opencl namespace
} // End annotations namespace
} // End insieme namespace

namespace std {
	ostream& operator<<(ostream& os, const insieme::annotations::opencl::Annotation& ann) {
		return ann.printTo(os);
	}
}
