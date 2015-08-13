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

#include <iostream>
#include <string>

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {


	class ProgressBar : public Printable {
		// the label of the progress bar
		std::string label;

		// the current progress state
		unsigned cur;

		// the total progress
		unsigned max;

		// the width in characters of the progress bar
		unsigned width;

		// whether progress increments should be automatically printed
		bool autoPrint;

		// the stream to print to
		std::ostream& out;

		mutable unsigned lastPrintedState;

	  public:
		ProgressBar(const std::string& label = "", unsigned max = 100, unsigned cur = 0, unsigned width = 60, std::ostream& out = std::cout,
		            bool autoPrint = true)
		    : label(label), cur(cur), max(max), width(width), autoPrint(autoPrint), out(out), lastPrintedState(width + 1) {}

		void inc(unsigned value = 1) {
			cur += value;
			if(autoPrint) { print(); }
		}

		std::ostream& printTo(std::ostream& out) const {
			unsigned a = getProgressBarState();

			// print label
			out << label;
			if(!label.empty()) { out << " "; }

			// print progress part
			unsigned i;
			out << "[";
			for(i = 0; i < a; i++) {
				out << "=";
			}
			if(a != width) {
				out << ">";
				i++;
			}
			for(; i < width; i++) {
				out << " ";
			}
			out << "] ";

			// print status summary
			out << format("%6.2f", (100.f * ((float)cur / (float)max))) << "\% of " << max << " ";

			// done
			return out << std::flush;
		}

	  private:
		unsigned getProgressBarState() const {
			unsigned res = ((float)cur / (float)max) * ((float)width);
			return (res <= width) ? res : width;
		}

		void print() const {
			auto state = getProgressBarState();

			// skip re-print if nothing has changed
			if(state == lastPrintedState) { return; }
			lastPrintedState = state;

			// clear old state
			out << "\r";

			// print bar
			printTo(out);
		}
	};
	//
	//
	///**
	// * some tool to print a progress bar, some day would be cool to have an infrastructure to do so
	// */
	// inline void printProgress (const std::string& prefix, unsigned cur, unsigned max){
	//	std::stringstream out;
	//	static unsigned last = 0;
	//	unsigned a = ((float)cur/ (float)max) * 60.0f;
	//	if (a > last){
	//		unsigned i;
	//		for (i = 0; i< a; i++)
	//			out << "=";
	//		out  << ">";
	//		i++;
	//		for (; i< 60; i++)
	//			out << " ";
	//		std::cout << "\r" << prefix << " [" << out.str() << "] " << boost::format("%5.2f") % (100.f*((float)cur/(float)max)) << "\% of " << max << " " <<
	// std::flush;
	//		last = a;
	//	}
	//	if (cur == max)
	//		last =0;
	//}

} // end namespace utils
} // end namespace insieme
