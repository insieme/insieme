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

#include "insieme/core/dump/text_dump.h"

#include <inttypes.h>
#include <limits>
#include <map>
#include <vector>
#include <cctype>

#include <boost/tokenizer.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace dump {


	namespace text {

		// The text format of the IR dump:
		//		(<NODE-TYPE> <VALUE>? <KNOWN-ANNOTATIONS>* |
		//			<CHILD-NODE-LIST>*
		//		)
		//
		// where the node-type is a text-representation of the node type,
		// the value the string representation of a potential represented
		// value (in case it is an encoded value node) and the child-list
		// is a recursive enumeration of all child nodes. Known annotations
		// will only be supported in a limited way.
		// The child-node list is a recursive enumeration of the child nodes
		// of the given node.


		namespace {

			// -- writer --

			/**
			 * A static visitor used to store node values within a stream.
			 */
			struct ValueDumper : public boost::static_visitor<void> {

				std::ostream& out;
				ValueDumper(std::ostream& out) : out(out) {}

				void operator()(bool value) const {
					out << ((value)?"true":"false");
				}

				void operator()(char value) const {
					out << "'" << value << "'";
				}

				void operator()(int value) const {
					out << value;
				}

				void operator()(unsigned value) const {
					out << value;
				}

				void operator()(const string& value) const {
					string mod = value;
					boost::algorithm::replace_all(mod, "\"", "\\\"");
					out << "\"" << mod << "\"";
				}
			};


			/**
			 * The text dumper is converting a given IR node into a text-based
			 * human readable format. The dumped structure represents the full
			 * IR tree and is not exploiting the sharing properties of the the DAG.
			 */
			class TextDumper {

			public:

				void dump(std::ostream& out, const NodePtr& ir) {
					dump(out, 0, ir);
				}

				void dump(std::ostream& out, int level, const NodePtr& cur) {
					// start with indentation and node type
					out << times("    ", level) << "(" << cur->getNodeType() << " ";

					if (cur->isValue()) {
						// dump value
						boost::apply_visitor(ValueDumper(out), cur->getNodeValue());
						out << ")";
						return;
					}
					// dump child nodes
					if (cur->getChildList().empty()) {
						out << ")";
						return;
					}

					// process child list
					out << "|";
					for_each(cur->getChildList(), [&](const NodePtr& child) {
						out << "\n";
						dump(out, level+1, child);
					});
					out << "\n" << times("    ", level) << ")";
				}
			};


			/**
			 * The tokenizer is splitting up the encoded form of an IR tree into its token. It
			 * is respecting the boundary tokens (, | and ) as well as text being escaped within
			 * quotes.
			 */
			struct TextEncodingTokenizer {

				/**
				 * A utility function testing for delimiter characters.
				 */
				bool isDelimiter(char x) const {
					return x=='(' || x==')' || x=='|';
				}

				/**
				 * Realizes the actual identification of the next token by searching
				 * its boundaries within the interval [next, end) and writing the result
				 * into the passed token.
				 */
				template <typename InputIterator, typename Token>
				bool operator()(InputIterator& next, InputIterator end, Token& tok) const {

					// a manipulation utility for the token
					typedef boost::tokenizer_detail::assign_or_plus_equal<
						typename boost::tokenizer_detail::get_iterator_category<InputIterator>::iterator_category
					> assigner;

					// clear the token
					assigner::clear(tok);

					// skip over white spaces
					while(next != end && isspace(*next)) {
						++next;
					}

					// check end-position
					if (next == end) {
						return false;
					}

					InputIterator start(next);

					// handle special characters
					if (isDelimiter(*next)) {
						assigner::plus_equal(tok,*next);
						assigner::assign(start, ++next, tok);
						return true;
					}

					// handle string literals
					char last = ' ';
					if (*next == '"') {
						assigner::plus_equal(tok,*next);
						++next;
						while(next != end && !(*next == '"' && last != '\\')) {
							assigner::plus_equal(tok,*next);
							last = *next;
							++next;
						}
						assigner::plus_equal(tok,*next);
						assigner::assign(start, ++next, tok);
						return true;
					}

					// handle rest
					while (next != end && !isspace(*next) && !isDelimiter(*next)) {
						assigner::plus_equal(tok,*next);
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
			 * The text loader is restoring an IR structure from a text-based ir encoding.
			 */
			class TextLoader {

				/**
				 * The type of the tokenizer used for parsing the encoded text.
				 */
				typedef boost::tokenizer<TextEncodingTokenizer, std::istreambuf_iterator<char>> tokenizer;
				typedef tokenizer::iterator token_iterator;

				/**
				 * The builder used to construct nodes.
				 */
				IRBuilder builder;

			public:

				TextLoader(NodeManager& manager) : builder(manager) {}

				/**
				 * Restores the node address stored within the given stream.
				 */
				NodeAddress load(std::istream& in) {

					// access the stream using an iterator
				    std::istreambuf_iterator<char> begin(in);
				    std::istreambuf_iterator<char> end;

				    TextEncodingTokenizer sep;
				    tokenizer tokens(begin,end, sep);

				    token_iterator nextToken = tokens.begin();
				    token_iterator tokenEnd = tokens.end();
				    NodePtr root = resolve(nextToken, tokenEnd);

				    // restore address
				    NodeAddress res(root);

				    // process path step by step
				    ++nextToken;
				    while(nextToken != tokenEnd) {
				    	res = res.getAddressOfChild(utils::numeric_cast<unsigned>(*nextToken));
				    	++nextToken;
				    }
				    return res;
				}

			private:

				/**
				 * A recursive descendant parser implementation reconstructing the tree
				 * encoded within the given token-stream.
				 */
				NodePtr resolve(token_iterator& cur, const token_iterator& end) const {
					assert(cur != end);

					// first token has to be a (
					auto token = *cur;
					if (token != "(") {
						throw InvalidEncodingException("Encountered unexpected token at begin of node: " + token);
					}

					// the next token is the node type => resolve type
					core::NodeType type = resolveType(*(++cur));

					// handle values
					if (type == NT_BoolValue || type == NT_CharValue ||
							type == NT_IntValue || type==NT_UIntValue || type == NT_StringValue) {

						// read the value token
						token = *(++cur);

						NodePtr res;

						// handle the individual values
						if (type == NT_BoolValue) {
							res = builder.boolValue(token[0] == 't' || token[0] == 'T' || token[0] == '1');
						} else if (type == NT_CharValue) {
							res = builder.charValue((token[0]=='\'')?token[1]:token[0]);
						} else if (type == NT_IntValue) {
							res = builder.intValue(utils::numeric_cast<int>(token));
						} else if (type == NT_UIntValue) {
							res = builder.uintValue(utils::numeric_cast<unsigned>(token));
						} else if (type == NT_StringValue) {
							if (token[0] == '\"') {
								token = token.substr(1,token.length()-2);
							}
							boost::algorithm::replace_all(token, "\\\"", "\"");
							res = builder.stringValue(token);
						}

						// consume closing bracket
						token = *(++cur);
						if (token != ")") {
							throw InvalidEncodingException("Encoding error - expecting ), encountered: " + token);
						}

						// return encoded value
						return res;
					}

					// handle standard node type
					vector<NodePtr> children;

					// consume pipe | token
					token = *(++cur);

					// see whether there have been child nodes
					if (token == ")") {
						// no child nodes
						return builder.get(type, children);
					}

					// process child list
					if (token != "|") {
						throw InvalidEncodingException("Encoding error - expecting | separator, encountered: " + token);
					}

					//  => while not at the end, collect child nodes
					++cur;
					while (cur != end && *cur != ")") {
						children.push_back(resolve(cur, end));
						++cur;
					}

					// check whether formatting is OK
					if (cur == end) {
						throw InvalidEncodingException("Encoding error - start and end brackets not matching!");
					}

					// build resulting node
					return builder.get(type, children);
				}

				/**
				 * A utility function mapping a node-name to its enumeration value.
				 */
				NodeType resolveType(const string& name) const {

					#define CONCRETE(NAME) \
						if (#NAME == name) { \
							return NT_##NAME; \
						}

					#include "insieme/core/ir_nodes.def"
					#undef CONCRETE

					std::cout << "WARNING: Unsupported node type: " << name << "\n";
					assert(false && "UNSUPPORTED node type!");

					// some standard result to avoid warnings
					return NT_CallExpr;
				}

			};

			void dumpPathIndices(std::ostream& out, const Path& path) {
				// dump in order
				if (path.getLength() > 2) {
					dumpPathIndices(out, path.getPathToParent());
				}
				if (path.getLength() > 1) {
					out << path.getIndex() << " ";
				}
			}

			void dumpPath(std::ostream& out, const Path& path) {
				// dump path
				if(path.getLength() > 1) {
					dumpPathIndices(out, path.getPathToParent());
				}
				out << path.getIndex();
			}

		}

		void dumpIR(std::ostream& out, const NodePtr& ir) {
			TextDumper().dump(out, ir);
		}

		void dumpAddress(std::ostream& out, const NodeAddress& address) {
			// just dump full IR tree ...
			dumpIR(out, address.getRootNode());

			// finish with a new-line
			out << "\n";

			// .. followed by the address path
			if (!address.isRoot()) {				// we can skip the address if it is the root
				dumpPath(out, address.getPath());
			}
		}


		NodePtr loadIR(std::istream& in, core::NodeManager& manager) {
			return loadAddress(in, manager).getAddressedNode();
		}

		NodeAddress loadAddress(std::istream& in, NodeManager& manager) {
			// just use text loader implementation
			return TextLoader(manager).load(in);
		}

	} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme
