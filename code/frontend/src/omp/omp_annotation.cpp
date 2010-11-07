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

#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/xml/xml_utils.h"
#include <memory>

namespace insieme {
namespace frontend {
namespace omp {

const core::StringKey<BaseAnnotation> BaseAnnotation::KEY("OpenMP");

const std::string BaseAnnotation::toString() const {
	std::ostringstream ss;
	for(AnnotationList::const_iterator it = getAnnotationListBegin(), end = getAnnotationListEnd(); it != end; ++it) {
		(*it)->dump(ss);
		if(it+1 != end)
			ss << "\\n";
	}
	return ss.str();
};

//xml::XmlElement& ompToXml(const BaseAnnotation& ann, xml::XmlElement& el) {
//	xml::XmlElement entries("pragmas", el.getDoc());
//	el << entries;
//
//	for (BaseAnnotation::AnnotationList::const_iterator it = ann.getAnnotationListBegin(); it != ann.getAnnotationListEnd(); ++it) {
//		xml::XmlElement entry("pragma", el.getDoc());
//		(*it)->toXml(entry);
//		entries << entry;
//	}
//
//	return el;
//}
//
//void Barrier::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "barrier");
//}
//
//void Master::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "master");
//}
//
//void Flush::toXml(insieme::xml::XmlElement& elem) {
////	elem << ("name", "flush");
//}
//
//void For::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "for");
//}
//
//void ParallelFor::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "parallel for");
//}
//
//void Section::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "section");
//}
//
//void Sections::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "sections");
//}
//
//void ParallelSections::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "parallel sections");
//}
//
//void Parallel::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "parallel");
//}
//
//void Critical::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "critical");
//}
//
//void Single::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "single");
//}
//
//void Atomic::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "atomic");
//}
//
//void Task::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "task");
//}
//
//void TaskWait::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "taskwait");
//}
//
//void Ordered::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "ordered");
//}
//
//void ThreadPrivate::toXml(insieme::xml::XmlElement& elem) {
////	elem.setAttr("name", "threadprivate");
//}
//
//std::shared_ptr<BaseAnnotation> ompFromXml(const xml::XmlElement& el) {
//	return std::make_shared<BaseAnnotation>( std::vector<AnnotationPtr>() );
//}
//
//XML_REGISTER_ANNOTATION(BaseAnnotation, "OmpAnnotation", ompToXml, ompFromXml);

} // End omp namespace
} // End frontend namespace
} // End insieme namespace
