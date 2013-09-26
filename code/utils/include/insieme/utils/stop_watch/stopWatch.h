
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
///////////////////////////////////////////////////////////////////////////////////
//
class StopWatch{

	class swImpl;
	static swImpl sw;

	public:

	////////////////////////////////
	//
	class swTicket {
		swImpl& sw;
	public:
		mutable bool valid;
		unsigned id;

		swTicket(swImpl& stopWatch, int num);
		swTicket(const swTicket& o);
		swTicket(swTicket&& o);
		~swTicket();

		void end();
	private:

	/// private assigment
		swTicket& operator==(swTicket& o);
	};

	StopWatch();
	~StopWatch();

	static swTicket start(const std::string& str);

	template <typename T, typename... Args>
	static swTicket start(const std::string& str, const T& value, const Args&... args){
		std::stringstream ss;
		ss << str << value;
		return StopWatch::start(ss.str(), args...);
	}
	static void 	stop (swTicket& swTicket);
	static void 	printStatus ();
};


typedef	chrono::time_point<chrono::high_resolution_clock> tTime;


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

////////////////////////////////////////////////////////////
/// this is the memory collector, any technique can be implemented here
int getMemUsage(){
	// TODO:
	return 0;
}

///////////////////////////////////////////////////////////
/// this is the time measure, any technique can be implemented here
tTime getTime(){
	return chrono::high_resolution_clock::now();
}

thread::id getThid(){
	return this_thread::get_id();
}


///////////////////////////////////////////////////////////////////////////////////
//

class StopWatch::swImpl{

	vector<tRecord> 	 measures;
	mutex staticLock;
	tTime  globalStart;

public:

	swImpl();
	~swImpl();
	swTicket getTicket(const string& str);
	void  	 endTiket(const swTicket& t);
	void 	 dump(ostream& os);

	friend class swTicket;
};

///////////////////////////////////////////////////////////////////////////////////
//   Tickets impl

StopWatch::swTicket& StopWatch::swTicket::operator==(swTicket& o){
	o.valid = false;
	return *this;
}

StopWatch::swTicket::swTicket(swImpl& stopWatch, int num)
:sw(stopWatch), valid(true), id(num)
{ }

StopWatch::swTicket::swTicket(const swTicket& o)
:sw(o.sw), valid(true), id(o.id)
{ o.valid =false;}

StopWatch::swTicket::swTicket(swTicket&& o)
:sw(o.sw), valid(true), id(o.id)
{}

StopWatch::swTicket::~swTicket(){
	//invalidate copy
	this->end();
}

void StopWatch::swTicket::end(){
	if(valid){
		sw.endTiket(*this);
		valid=false;
	}
}


///////////////////////////////////////////////////////////////////////////////////
//   stopwatch implementation

StopWatch::swImpl::swImpl() 
:	globalStart (getTime())
{
	tRecord record(getThid(), "GlobalScope", getMemUsage(), globalStart);
	measures.insert(measures.end(), record);
}

StopWatch::swImpl::~swImpl(){
	// update global scope record
	measures[0].update(getMemUsage(), getTime());
	// print status to file
 	ofstream myfile;
   	myfile.open ("times.sw");
	dump(myfile);
	myfile.close();
}


StopWatch::swTicket StopWatch::swImpl::getTicket(const string& str){
	tRecord record(getThid(), str, getMemUsage(), getTime());
	staticLock.lock();
	measures.insert(measures.end(), record);
	unsigned id = measures.size()-1;
	staticLock.unlock();
	swTicket ticket(*this, id);
	return ticket;
}

void StopWatch::swImpl::endTiket(const swTicket& swT){
	measures[swT.id].update(getMemUsage(), getTime());
}

void StopWatch::swImpl::dump(ostream& os){
	os << "#microseconds precission (1/ 1 000 000 )" << endl;
	os << "#[pid]\t[start]\t[end]\t[name]\t[time]" << endl;
	for (const tRecord& r : measures){
		r.dump(os, globalStart);
	}
}

// instantiate static member
StopWatch::swImpl StopWatch::sw;


///////////////////////////////////////////////////////////////////////////////////
//		Static interface

StopWatch::swTicket StopWatch::start(const string& str){
	return sw.getTicket(str);
}

void StopWatch::stop (StopWatch::swTicket& swTicket){
	swTicket.end();
}

void	StopWatch::printStatus (){
	cout << "****************** STOPWATCH TIMES *******************" << endl;
	sw.dump(cout);
	cout << "****************** STOPWATCH TIMES *******************" << endl;
}


} //utils namespace
} // insieme namespace

