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
#include "insieme/transform/dump/text_dump.h"

#include <map>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/transform/filter/standard_filter.h"

#include <boost/tokenizer.hpp>

namespace insieme {
namespace transform {
namespace dump {

	namespace {

		std::map<string, filter::TargetFilter> initTargetFilterMap() {
			std::map<string, filter::TargetFilter> res;

			res.insert(std::make_pair("outermost_loops", filter::outermostLoops()));
			res.insert(std::make_pair("innermost_loops", filter::innermostLoops()));
			res.insert(std::make_pair("2nd_innermost_loops", filter::innermostLoops(2)));
			res.insert(std::make_pair("3rd_innermost_loops", filter::innermostLoops(3)));

			return res;
		}

		const filter::TargetFilter& getTargetFilter(const string& name) {
			static const std::map<string, filter::TargetFilter> index = initTargetFilterMap();
			auto pos = index.find(name);
			assert(pos != index.end() && "Unsupported filter encountered!");
			return pos->second;
		}

		const string& getTargetFilterName(const filter::TargetFilter& filter) {
			static const std::map<string, filter::TargetFilter> index = initTargetFilterMap();
			static const string unsupported = "TARGET_FILTER_NOT_SUPPORTED";
			for(auto it = index.begin(); it != index.end(); ++it) {
				if(it->second == filter) { return it->first; }
			}
			return unsupported;
		}


		void dumpTransformInternal(std::ostream& out, const TransformationPtr& transform, const Indent& indent);

		/**
		 * A static visitor used to print values of parameters to a given output streams.
		 */
		struct ValuePrinter : public boost::static_visitor<std::ostream&> {
			std::ostream& out;
			Indent indent;

			ValuePrinter(std::ostream& out, const Indent& indent) : out(out), indent(indent) {}

			// handle the general case using a template
			template <typename S>
			std::ostream& operator()(const S& cur) const {
				return out << cur;
			}

			// specialize for some particular cases
			std::ostream& operator()(const bool value) const {
				return out << ((value) ? "true" : "false");
			}

			std::ostream& operator()(const TransformationPtr& transform) const {
				dumpTransformInternal(out, transform, indent);
				return out;
			}

			std::ostream& operator()(const filter::Filter& filter) const {
				//				assert_fail() << "Not implemented yet!";
				return out << " FILTER_NOT_SUPPORTED ";
			}

			std::ostream& operator()(const filter::TargetFilter& filter) const {
				return out << getTargetFilterName(filter);
			}

			std::ostream& operator()(const vector<parameter::Value>& values) const {
				bool transformIncluded = false;
				out << "(" << join(", ", values, [&](std::ostream& out, const parameter::Value& cur) {
					bool isTransform = parameter::isTypeOf<TransformationPtr>(cur);
					if(isTransform) { out << "\n" << indent + 1; }
					boost::apply_visitor(ValuePrinter(out, indent + 1), cur);
					transformIncluded = transformIncluded || isTransform;
				});
				if(transformIncluded) { out << "\n" << indent; }
				return out << ")";
			}
		};


		void dumpTransformInternal(std::ostream& out, const TransformationPtr& transform, const Indent& indent) {
			// start by printing the name
			out << transform->getType().getName();

			// continue by listing the parameters
			const parameter::Value& value = transform->getParameters();
			bool isList = parameter::isTypeOf<vector<parameter::Value>>(value);
			if(!isList) { out << "("; }
			boost::apply_visitor(ValuePrinter(out, indent), value);
			if(!isList) { out << ")"; }
		}
	}


	void dumpTransformation(std::ostream& out, const TransformationPtr& transform) {
		dumpTransformInternal(out, transform, Indent());
	}

	void dumpTransformations(std::ostream& out, const vector<TransformationPtr>& transformations) {
		int counter = 0;
		for_each(transformations, [&](const TransformationPtr& cur) {
			out << "#Transformation " << ++counter << ":\n";
			dumpTransformation(out, cur);
			out << "\n\n";
		});
	}


	namespace {

		/**
		 * The tokenizer is splitting up the encoded form of an expression into its tokens. It
		 * is respecting the boundary tokens ( ) as well as comments starting with #.
		 */
		struct TextTokenizer {
			/**
			 * A utility function testing for delimiter characters.
			 */
			bool isDelimiter(char x) const {
				return x == '(' || x == ')' || x == ',';
			}

			bool isSpace(char x) const {
				return isspace(x) || x == ',';
			}

			/**
			 * Realizes the actual identification of the next token by searching
			 * its boundaries within the interval [next, end) and writing the result
			 * into the passed token.
			 */
			template <typename InputIterator, typename Token>
			bool operator()(InputIterator& next, InputIterator end, Token& tok) const {
				// a manipulation utility for the token
				typedef boost::tokenizer_detail::assign_or_plus_equal<typename boost::tokenizer_detail::get_iterator_category<InputIterator>::iterator_category>
				    assigner;

				// clear the token
				assigner::clear(tok);

				// skip over white spaces and comments
				bool isComment = false;
				while(next != end && (isSpace(*next) || *next == '#' || isComment)) {
					if(isComment && (*next == '\n' || *next == '#')) { isComment = false; }
					if(!isComment && *next == '#') { isComment = true; }
					++next;
				}

				// check end-position
				if(next == end) { return false; }

				InputIterator start(next);

				// handle special characters
				if(isDelimiter(*next)) {
					assigner::plus_equal(tok, *next);
					assigner::assign(start, ++next, tok);
					return true;
				}

				// handle rest
				while(next != end && !isSpace(*next) && !isDelimiter(*next)) {
					assigner::plus_equal(tok, *next);
					++next;
				}
				assigner::assign(start, next, tok);
				return true;
			}

			void reset() const {
				// no internal state
			}
		};


		/**
		 * The text loader is restoring a Transformation from a text-based representation
		 */
		class TextLoader {
			/**
			 * The type of the tokenizer used for parsing the encoded text.
			 */
			typedef boost::tokenizer<TextTokenizer, std::istreambuf_iterator<char>> tokenizer;
			typedef tokenizer::iterator token_iterator;


			struct ValueParser {
				token_iterator& next;
				const token_iterator& end;
				const TextLoader& loader;

				ValueParser(token_iterator& next, const token_iterator& end, const TextLoader& loader) : next(next), end(end), loader(loader) {}


				parameter::Value operator()(const parameter::BoolParameterPtr&) {
					string value = *(next++);
					return parameter::makeValue(value[0] == 'T' || value[0] == 't' || value[0] == '1');
				}

				parameter::Value operator()(const parameter::IntParameterPtr&) {
					return parameter::makeValue(utils::numeric_cast<int>(*(next++)));
				}

				parameter::Value operator()(const parameter::UIntParameterPtr&) {
					return parameter::makeValue(utils::numeric_cast<unsigned>(*(next++)));
				}

				parameter::Value operator()(const parameter::TransformationParameterPtr&) {
					return loader.resolve(next, end);
				}

				parameter::Value operator()(const parameter::FilterParameterPtr&) {
					// TODO: implement!
					++next;
					return parameter::makeValue(filter::all);
				}

				parameter::Value operator()(const parameter::TargetFilterParameterPtr&) {
					// just look up list of registered filters
					return parameter::makeValue(getTargetFilter(*(next++)));
				}

				parameter::Value operator()(const parameter::ListParameterPtr& type) {
					// consume opening token
					if(*(next++) != "(") { assert_fail() << "No opening bracket!"; }
					vector<parameter::Value> res;
					while(*next != ")") {
						res.push_back(loader.resolveValue(next, end, type->getElementType()));
					}
					++next;
					return parameter::makeValue(res);
				}

				parameter::Value operator()(const parameter::TupleParameterPtr& type) {
					// consume opening token
					if(*(next++) != "(") { assert_fail() << "No opening bracket!"; }
					vector<parameter::Value> res;
					for_each(type->getComponents(), [&](const parameter::ParameterPtr& cur) { res.push_back(loader.resolveValue(next, end, cur)); });
					if(*(next++) != ")") { assert_fail() << "No closing bracket!"; }
					return parameter::makeValue(res);
				}
			};

			/**
			 * The catalog used for restoring transformations.
			 */
			const Catalog& catalog;

		  public:
			TextLoader(const Catalog& catalog) : catalog(catalog) {}

			/**
			 * Restores the transformation stored within the given stream.
			 */
			vector<TransformationPtr> load(std::istream& in) {
				// access the stream using an iterator
				std::istreambuf_iterator<char> begin(in);
				std::istreambuf_iterator<char> end;

				TextTokenizer sep;
				tokenizer tokens(begin, end, sep);

				token_iterator nextToken = tokens.begin();
				token_iterator tokenEnd = tokens.end();

				// load sequence of transformations
				vector<TransformationPtr> res;
				while(nextToken != tokenEnd) {
					res.push_back(resolve(nextToken, tokenEnd));
				}
				return res;
			}

		  private:
			TransformationPtr resolve(token_iterator& cur, const token_iterator& end) const {
				assert(cur != end);

				// transformations start with the name of a transformation
				string name = *(cur++);
				TransformationTypePtr type = catalog.getTransformationType(name);

				// TODO: use an exception to indicate parsing errors
				assert_true(type) << "Unknown transformation type encountered!";

				auto paramInfos = type->getParameterInfo();
				if(paramInfos->isAtomic()) {
					// consume opening bracket
					if(*(cur++) != "(") { /* TODO: throw exception */ assert_fail(); };

					// load value
					parameter::Value value = resolveValue(cur, end, paramInfos);

					// consume closing bracket
					if(*(cur++) != ")") { /* TODO: throw exception */ assert_fail(); };

					// build transformation
					return type->createTransformation(value);
				}
				return type->createTransformation(resolveValue(cur, end, paramInfos));
			}

			parameter::Value resolveValue(token_iterator& cur, const token_iterator& end, const parameter::ParameterPtr& param) const {
				// the value construction is conducted by the value parser
				ValueParser parser(cur, end, *this);
				return parameter::createValue(param, parser);
			}
		};
	}


	TransformationPtr loadTransformation(std::istream& in, const Catalog& catalog) {
		auto res = loadTransformations(in, catalog);
		if(res.empty()) { return TransformationPtr(); }
		return res[0];
	}

	vector<TransformationPtr> loadTransformations(std::istream& in, const Catalog& catalog) {
		return TextLoader(catalog).load(in);
	}


} // end namespace dump
} // end namespace transform
} // end namespace insieme
