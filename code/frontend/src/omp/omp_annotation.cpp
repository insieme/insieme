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

#include "omp/omp_annotation.h"

#include "xml_utils.h"
#include <memory>

namespace insieme {
namespace frontend {
namespace omp {
namespace annotation {

const core::StringKey<OmpBaseAnnotation> OmpBaseAnnotation::KEY("OpenMP");


xml::XmlElement& ompToXml(const OmpBaseAnnotation& ann, xml::XmlElement& el) {
	xml::XmlElement entries("pragmas", el.getDoc());
	el << entries;

	for (OmpBaseAnnotation::OmpAnnotationList::const_iterator it = ann.getListBegin(); it != ann.getListEnd(); ++it) {
		xml::XmlElement entry("pragma", el.getDoc());
		(*it)->toXml(entry);
		entries << entry;
	}

	return el;
}

void OmpBarrier::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "barrier");
}

void OmpMaster::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "master");
}

void OmpFlush::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "flush");
}

void OmpFor::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "for");
}

void OmpParallelFor::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "parallel for");
}

void OmpSection::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "section");
}

void OmpSections::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "sections");
}

void OmpParallelSections::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "parallel sections");
}

void OmpParallel::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "parallel");
}

void OmpCritical::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "critical");
}

void OmpSingle::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "single");
}

void OmpAtomic::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "atomic");
}

void OmpTask::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "task");
}

void OmpTaskWait::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "taskwait");
}

void OmpOrdered::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "ordered");
}

void OmpThreadPrivate::toXml(insieme::xml::XmlElement& elem) {
	elem.setAttr("name", "threadprivate");
}

std::shared_ptr<OmpBaseAnnotation> ompFromXml(const xml::XmlElement& el) {
	return std::make_shared<OmpBaseAnnotation>( std::vector<OmpAnnotationPtr>() );
}

XML_CONVERTER(OmpBaseAnnotation, "OmpAnnotation", ompToXml, ompFromXml);

} // End annotation namespace
} // End omp namespace
} // End frontend namespace
} // End insieme namespace
