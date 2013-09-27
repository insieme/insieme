
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
#include <sstream>  
#include <fstream>
#include <chrono>
#include <thread>
#include <vector>

#define STOPWATCH(name, expr)\
{ \
	StopWatch::swTicket wsT#name = StopWatch::start(#name); \
	expr;\
	StopWatch::stop(swT#name);\
}

namespace insieme{
namespace utils{

using namespace std;

typedef	chrono::time_point<chrono::high_resolution_clock> tTime;


////////////////////////////////////////////////////////////
/// this is the memory collector, any technique can be implemented here
inline int getMemUsage(){
	// TODO:
	return 0;
}

/**
 * high preccission timer
 */
inline tTime getTime(){
	return chrono::high_resolution_clock::now();
}

/**
 * retrieve the thread that started this stopwatch. since the stopwatches are scoped
 * it should not be posible to transfer it to another thread.
 * the std:: thread id operation should be low level enough to deal with different kind
 * of shared memory programing models
 */
inline thread::id getThid(){
	return this_thread::get_id();
}


/**
 * stopwatch class, is a singleton which allows to start and stop timers, 
 * since is centraliced in a singleton is thread safe and can store the whole program status
 */
class StopWatch{
	class swImpl;

	public:
	////////////////////////////////
	//
	class swTicket {
		StopWatch::swImpl& sw;
	public:
		mutable bool valid;
		unsigned id;

		swTicket(StopWatch::swImpl& stopWatch, int num)
		:sw(stopWatch), valid(true), id(num)
		{ }

		swTicket(const swTicket& o)
		:sw(o.sw), valid(true), id(o.id)
		{ o.valid =false;}

		swTicket(swTicket&& o)
		:sw(o.sw), valid(true), id(o.id)
		{}

		~swTicket(){
			//invalidate copy
			this->end();
		}

		void end(){
			if(valid){
				sw.endTiket(*this);
				valid=false;
			}
		}
	private:

	/// private assigment
		swTicket& operator=(swTicket& o){
			o.valid = false;
			return *this;
		}

	};

	private:

	class swImpl{
		
		class tRecord{
		public:
			thread::id thid;
			string name;
			double mem;
			std::chrono::microseconds time;

			tTime start;
			tTime end;


			tRecord(thread::id pid, const string& n, double m, tTime t)
			: thid(pid), name(n), mem(m), start(t)
			{ }

			void update(double m, tTime t){
				end = t;
				mem = m-mem;
				time = t-start;
			}

			void dump(ostream& os, tTime globalStart)const{
				std::chrono::microseconds st = start-globalStart;
				std::chrono::microseconds nd = end-globalStart;
				os << thid << "\t" << st.count() << "\t" << nd.count() << "\t" << name << "\t"<< time.count() << endl;
			}
		};

		vector<tRecord> 	 measures;
		mutex staticLock;
		tTime  globalStart;

		public:

		swImpl()
		:	globalStart (getTime())
		{
			tRecord record(getThid(), "GlobalScope", getMemUsage(), globalStart);
			measures.insert(measures.end(), record);
		}

		~swImpl(){
			// update global scope record
			measures[0].update(getMemUsage(), getTime());
			// print status to file
			ofstream myfile;
			myfile.open ("times.sw");
			dump(myfile);
			myfile.close();
		}

		swTicket getTicket(const string& str){
			tRecord record(getThid(), str, getMemUsage(), getTime());
			staticLock.lock();
			measures.insert(measures.end(), record);
			unsigned id = measures.size()-1;
			staticLock.unlock();
			swTicket ticket(*this, id);
			return ticket;
		}

		void  	 endTiket(const swTicket& swT){
			measures[swT.id].update(getMemUsage(), getTime());
		}

		void 	 dump(ostream& os){
			os << "#microseconds precission (1/ 1 000 000 )" << endl;
			os << "#[pid]\t[start]\t[end]\t[name]\t[time]" << endl;
			for (const tRecord& r : measures){
				r.dump(os, globalStart);
			}
		}

		friend class swTicket;
	} sw;


	public:

	/**
	 * generates a new stopwatch point with a given name. 
	 */
	static swTicket start(const std::string& str){
		return getInstance().sw.getTicket(str);
	}

	/**
	 * allow to compose formated names, to parametrize the string 
	 */
	template <typename T, typename... Args>
	static swTicket start(const std::string& str, const T& value, const Args&... args){
		std::stringstream ss;
		ss << str << value;
		return StopWatch::start(ss.str(), args...);
	}
	/** 
	 * stops a stopwatch ticket
	 */
	static void 	stop (swTicket& swTicket){
		swTicket.end();
	}

	/**
	 * prints the current status of all tracked stopwatches, 
	 * any non finished timer will show random end time and 0 duration
	 */
	static void printStatus(ostream& os = std::cout){
		os << "=============== STOPWATCH status =======================" << std::endl;
		getInstance().sw.dump(os);
		os << "========================================================" << std::endl;
	}

	// singleton stuff
	private:
	static StopWatch& getInstance(){
		static StopWatch singleton;
		return singleton;
	}

	StopWatch(){};
	~StopWatch(){};
};


} //utils namespace
} // insieme namespace

