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
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <typeindex>

#include "insieme/core/ir.h"
#include "insieme/core/ir_node_annotation.h"

namespace insieme {
namespace core {
namespace dump {

	// forward declarations
	class AnnotationConverter;
	typedef std::shared_ptr<AnnotationConverter> AnnotationConverterPtr;


	/**
	 * A base class for all annotation converter implementations. The main
	 * task of an annotation converter is to be named and to encode annotations
	 * into IR data structures and to decode them again.
	 */
	class AnnotationConverter {
		/**
		 * The name of this converter.
		 */
		const string name;

	  public:
		/**
		 * Creates a new converter based on the given name. The given
		 * name should be unique throughout the system. The name is used within the
		 * binary dump-file to record the converter used to map annotations.
		 */
		AnnotationConverter(const string& name) : name(name) {}

		/**
		 * A virtual destructor doing nothing.
		 */
		virtual ~AnnotationConverter() {}

		/**
		 * Obtains the unique identifier name of this converter.
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Requests the given annotation to be encoded into an IR structure. If the
		 * given annotation can not be converted by this converter, a NULL pointer shell be
		 * returned.
		 *
		 * @param manager the node manager to be used for the conversion process
		 * @param annotation the annotation to be encoded into an IR structure
		 * @return the result of the encoding or NULL in case the given annotation can not
		 * 			be encoded by this converter
		 */
		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const = 0;

		/**
		 * Requests the given IR to be decoded into an annotation. If the given node
		 * does not represent a proper encoding of any supported annotation, a NULL pointer
		 * shell be returned.
		 *
		 * @param node the node encoding the annotation to be decoded
		 * @return the decoded annotation or NULL if no valid annotation has been encoded
		 */
		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const = 0;

		/**
		 * Requests an annotation to be decoded and attached to a given target node.
		 *
		 * @param target the node the decoded annotation shell be attached to
		 * @param annotation the annotation to be decoded
		 */
		void attachAnnotation(const NodePtr& target, const ExpressionPtr& annotation) const {
			if(!target) { return; }
			auto res = toAnnotation(annotation);
			if(res) { res->clone(res, target); }
		}
	};

	/**
	 * A class realizing a annotation converter register. This kind of register is capable
	 * of indexing annotation converter instances with names and annotation types they are
	 * capable of handling.
	 */
	class AnnotationConverterRegister {
		/**
		 * The index linking converters to the types they are capable of handling (required
		 * during the store process).
		 */
		std::map<std::type_index, AnnotationConverterPtr> type_index;

		/**
		 * The index linking converters to their names (used for obtaining converters during
		 * the load process)
		 */
		std::map<std::string, AnnotationConverterPtr> name_index;

	  public:
		/**
		 * Obtains the default register (singlton) to be used by default during the conversion process.
		 */
		static AnnotationConverterRegister& getDefault();

		/**
		 * Registers the given converter for the given type of annotation.
		 *
		 * @param converter the converter instance to be registered
		 * @param index the type index of the element to be registered
		 * @return true if registration has been successful, false otherwise
		 */
		bool registerConverter(const AnnotationConverterPtr& converter, const std::type_index& index);

		/**
		 * Registers the given converter for the given type of annotation.
		 *
		 * @tparam C the converter instance to be registered
		 * @tparam A the type of annotation to be handled by the given converter
		 * @return true if registration has been successful, false otherwise
		 */
		template <typename C, typename A>
		bool registerConverter() {
			return registerConverter(std::make_shared<C>(), typeid(A));
		}

		/**
		 * Requests the converter linked to the given name.
		 *
		 * @param name the name of the converter to be looking for
		 * @return a pointer to the obtained converter, a NULL pointer if no such converter is available
		 */
		const AnnotationConverterPtr& getConverterFor(const std::string& name) const;

		/**
		 * Requests the converter linked to the given annotation.
		 *
		 * @param annotation the annotation for which a converter is requested
		 * @return a pointer to the obtained converter, a NULL pointer if no such converter is available
		 */
		const AnnotationConverterPtr& getConverterFor(const NodeAnnotationPtr& annotation) const;
	};


	/**
	 * A macro to be used for registering a annotation type converter to be used by the IR dump mechanisms.
	 */
	#define REGISTER_ANNOTATION_CONVERTER(_CONVERTER, _ANNOTATION_TYPE)                                                                                        \
		namespace {                                                                                                                                            \
			bool __attribute__((unused)) _reg##_CONVERTER =                                                                                                    \
			    insieme::core::dump::AnnotationConverterRegister::getDefault().registerConverter<_CONVERTER, _ANNOTATION_TYPE>();                              \
		}

	/**
	 * A macro creating the boyler-plate code for a annotation converter including the registration
	 * within the default converter register.
	 */
	#define ANNOTATION_CONVERTER(_ANNOTATION_NAME)                                                                                                             \
		struct _ANNOTATION_NAME##Converter;                                                                                                                    \
		REGISTER_ANNOTATION_CONVERTER(_ANNOTATION_NAME##Converter, _ANNOTATION_NAME);                                                                          \
		struct _ANNOTATION_NAME##Converter : public insieme::core::dump::AnnotationConverter {                                                                 \
			_ANNOTATION_NAME##Converter() : insieme::core::dump::AnnotationConverter(#_ANNOTATION_NAME "Converter"){};

	#define ANNOTATION_CONVERTER_END };

	/**
	 * A macro similar to the ANNOTATION_CONVERTER macro specialized for value annotations.
	 *
	 * @see ANNOTATION_CONVERTER
	 */
	#define VALUE_ANNOTATION_CONVERTER(_VALUE_TYPE)                                                                                                            \
		struct _VALUE_TYPE##AnnotationConverter;                                                                                                               \
		REGISTER_ANNOTATION_CONVERTER(_VALUE_TYPE##AnnotationConverter, core::value_node_annotation<_VALUE_TYPE>::type);                                       \
		struct _VALUE_TYPE##AnnotationConverter : public insieme::core::dump::AnnotationConverter {                                                            \
			_VALUE_TYPE##AnnotationConverter() : insieme::core::dump::AnnotationConverter(#_VALUE_TYPE "AnnotationConverter"){};

	#define VALUE_ANNOTATION_CONVERTER_END };

} // end namespace dump
} // end namespace core
} // end namespace insieme
