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

#include <memory>

#include "insieme/utils/automata/automata.h"


namespace insieme {
namespace utils {
namespace automata {

	/**
	 * A manipulation wrapper for automates enabling the application
	 * of regular expression like connectives.
	 */
	template <typename Pattern = char, typename Matcher = std::equal_to<Pattern>>
	class RegularLanguage {
		/**
		 * A typedef for the state type.
		 */
		typedef typename Automata<Pattern, Matcher>::state_type State;

		/**
		 * The automata used to recognize the language of this regex.
		 * It is stored using a shared pointer since the automata is not copyable.
		 */
		std::shared_ptr<Automata<Pattern, Matcher>> automata;

	  public:
		/**
		 * Create a regular language consisting solely of the given pattern.
		 *
		 * @param pattern the only word within the language
		 */
		RegularLanguage(const Pattern& pattern) {
			automata = std::make_shared<Automata<Pattern, Matcher>>();

			// create an automata accepting a single pattern
			State s1 = automata->getInitialState();
			State s2 = automata->getNewState();

			automata->addTransition(s1, pattern, s2);
			automata->setFinalState(s2);
		}

		/**
		 * Obtains access to the internally stored automata.
		 */
		Automata<Pattern, Matcher>& getAutomata() {
			return *automata;
		}

		/**
		 * Obtains access to the internally stored automata.
		 */
		const Automata<Pattern, Matcher>& getAutomata() const {
			return *automata;
		}
	};

	/**
	 * Creates a new regular language consisting of solely the given word / pattern.
	 *
	 * @param pattern the pattern to be covered by the resulting language.
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>>
	RegularLanguage<Pattern, Matcher> single(const Pattern& pattern) {
		return RegularLanguage<Pattern, Matcher>(pattern);
	}

	/**
	 * Connects the two given languages and creates a new language covering
	 * all sequences a'b' where a' was part of the first language and b' part of
	 * the second language.
	 *
	 * @param langA the first language, will be invalidated in the process
	 * @param langB the second language, will also be invalidated in the process
	 * @return a new language forming the concatenation of the given languages
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>>
	RegularLanguage<Pattern, Matcher> sequence(RegularLanguage<Pattern, Matcher> langA, RegularLanguage<Pattern, Matcher> langB) {
		typedef typename Automata<Pattern, Matcher>::state_type State;

		Automata<Pattern, Matcher>& a1 = langA.getAutomata();
		Automata<Pattern, Matcher>& a2 = langB.getAutomata();

		// extract start/stop states
		State s1 = a1.getInitialState();
		State s2 = a2.getInitialState();

		State f1 = *a1.getFinalStates().begin();
		State f2 = *a2.getFinalStates().begin();

		// copy states
		a1.import(a2);

		// concatenate automata
		a1.addEpsilonTransition(f1, s2);

		// set up new start/end states
		a1.setInitialState(s1);
		a1.setFinalState(f2);

		return langA;
	}

	/**
	 * A connector supporting the concatenation of multiple languages using
	 * consecutive applications of the sequence operator.
	 *
	 * @param langA the first language, will be invalidated in the process
	 * @param langB the second language, will also be invalidated in the process
	 * @param rest the remaining languages
	 * @return a new language forming the concatenation of the given languages
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>, typename... L>
	RegularLanguage<Pattern, Matcher> sequence(RegularLanguage<Pattern, Matcher> langA, RegularLanguage<Pattern, Matcher> langB, L... rest) {
		return sequence(sequence(langA, langB), rest...);
	}


	/**
	 * A connector forming the union of the given languages.
	 *
	 * @param langA the first language
	 * @param langB the second language
	 * @return a language accepting every word of both languages.
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>>
	RegularLanguage<Pattern, Matcher> alternativ(RegularLanguage<Pattern, Matcher> langA, RegularLanguage<Pattern, Matcher> langB) {
		typedef typename Automata<Pattern, Matcher>::state_type State;

		Automata<Pattern, Matcher>& a1 = langA.getAutomata();
		Automata<Pattern, Matcher>& a2 = langB.getAutomata();

		// extract start/stop states
		State s1 = a1.getInitialState();
		State s2 = a2.getInitialState();

		State f1 = *a1.getFinalStates().begin();
		State f2 = *a2.getFinalStates().begin();

		// copy states
		a1.import(a2);

		// create new start / final state
		State s = a1.getNewState();
		State f = a1.getNewState();

		a1.setInitialState(s);
		a1.setFinalState(f);

		// link it
		a1.addEpsilonTransition(s, s1);
		a1.addEpsilonTransition(s, s2);
		a1.addEpsilonTransition(f1, f);
		a1.addEpsilonTransition(f2, f);

		return langA;
	}

	/**
	 * A connector supporting the combination of multiple languages using
	 * consecutive applications of the alternative operator.
	 *
	 * @param langA the first language, will be invalidated in the process
	 * @param langB the second language, will also be invalidated in the process
	 * @param rest the remaining languages
	 * @return a new language forming the union of the given languages
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>, typename... L>
	RegularLanguage<Pattern, Matcher> alternativ(RegularLanguage<Pattern, Matcher> langA, RegularLanguage<Pattern, Matcher> langB, L... rest) {
		return alternativ(alternativ(langA, langB), rest...);
	}


	/**
	 * A connector creating a language of all words representing repetitions
	 * of the given language.
	 *
	 * @param lang the language to be repeated
	 * @return the requested language.
	 */
	template <typename Pattern, typename Matcher = std::equal_to<Pattern>>
	RegularLanguage<Pattern, Matcher> repetition(RegularLanguage<Pattern, Matcher> lang) {
		typedef typename Automata<Pattern, Matcher>::state_type State;

		Automata<Pattern, Matcher>& a = lang.getAutomata();

		// extract start/stop states
		State s = a.getInitialState();
		State f = *a.getFinalStates().begin();

		// create new start and end state
		State ns = a.getNewState();
		State nf = a.getNewState();

		a.setInitialState(ns);
		a.setFinalState(nf);

		// add new transitions
		a.addEpsilonTransition(ns, s);
		a.addEpsilonTransition(f, nf);
		a.addEpsilonTransition(f, s);
		a.addEpsilonTransition(ns, nf);

		return lang;
	}


} // end namespace automata
} // end namespace utils
} // end namespace insieme
