
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


#define _GLIBCXX_USE_NANOSLEEP
#include <functional>
#include <iostream>
#include <vector>
#include <tuple>
#include <future>
#include <chrono>
#include <thread>
#include <list>


namespace insieme {
namespace utils {

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	struct TaskBase {

		mutable std::vector<TaskBase*> dependencies;

		bool done;

		TaskBase() : done(false) {}

		virtual ~TaskBase() { };

		TaskBase& operator>>(TaskBase& task) {
			task.dependencies.push_back(this);
			return task;
		}

		virtual void operator()() =0;
	};

	namespace {

		template<unsigned N>
		struct apply_tuple {
			template<typename F, typename T, typename ... Args>
			static void on( const F& fun, const T& tuple, const Args& ... args ) {
				apply_tuple<N-1>::on(fun, tuple, std::get<N-1>(tuple), args...);
			}
		};

		template<>
		struct apply_tuple<0> {
			template<typename F, typename T, typename ... Args>
			static void on( const F& fun, const T& tuple, const Args& ... args ) {
				fun(args ...);
			}
		};
	}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



	/**
	 *	its dutty is to get the work done,
	 *	it should be purelly private, so only tasks can call it
	 */
	class TaskManager{

		std::list<TaskBase*> submitted;
		std::mutex lock;

		class Worker{
			TaskManager& mgr;
			bool done;

			Worker(TaskManager& m) : mgr(m), done(false){
			}

			void operator()(){
				while (!done){
					while (!mgr.submitted.empty()){
						auto tsk = mgr.getTask();
						(*tsk)();
					}
				}
			}

			void finish (){
				done =true;
			}
		};

		// HERE: how to model this? 
		// I need the thread to call join on ending
		// i need also the obj to call finish
		std::vector<std::thread> workers;

		////////////////////////////////////////////////
		//
		////////////////////////////////////////////////

		TaskManager()
		{
			for (int i = 0; i < 7; ++i){
				//workers.push_back()
			}
		}

		inline void processTask(TaskBase* t){
			(*t)();
		}

		TaskBase* getTask(){
			TaskBase* t = nullptr;
			lock.lock();
			if (!submitted.empty()){
				t = submitted.front();
				submitted.pop_front();
			}
			lock.unlock();
			return t;
		}

		////////////////////////////////////////////////
		// Static interface
		////////////////////////////////////////////////
		
		static void addTask(TaskBase* t){
			TaskManager& mgr =getInstance();
			mgr.lock.lock();
			mgr.submitted.push_back(t);
			mgr.lock.unlock();
		}

		static void wait (TaskBase* t){
			TaskManager& mgr = getInstance();

			// while the task i wait for is still in the queue, 
			// this thread is also a worker, therefore works
			mgr.lock.lock();
			while (std::find (mgr.submitted.begin(), mgr.submitted.end(), t) != mgr.submitted.end()){
				mgr.lock.unlock();
				// work a little...
				auto tsk = mgr.getTask();
				(*tsk)();

				mgr.lock.lock();
			}
			mgr.lock.unlock();
		}

		static TaskManager& getInstance(){
			static TaskManager inst;
			return inst;
		}



		friend class TaskBase;
	};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<typename R, typename ... Params>
	class Task : public TaskBase {

		typedef std::function<R(Params ...)> fun_t;

		fun_t fun;

		std::tuple<Params...> args;

		mutable bool owner;

	public:

		Task(const fun_t& fun, const Params& ... args) : fun(fun), args(args ...), owner(true){}


		Task(const Task<R, Params...>& o)
		: fun(o.fun), args(args), owner(true) { 
			o.owner=false; 
		}

		Task<R, Params...> operator=(const Task<R, Params...>& o) = delete;

		~Task(){
			if (owner) (*this)();
		}

		//////////////////////////////////////////
		// operation
		void operator()() {
			// make sure it hasn't been processed before and is only processed once (DAG)
			if (done) return;
			done = true;

			// process dependencies concurrently
			std::vector<std::future<void>> futures;
			for(auto cur : dependencies) {
				futures.push_back(std::async(std::launch::async, [cur](){ (*cur)(); }));
//				futures.push_back(std::async([cur](){ (*cur)(); }));
			}

			// wait for dependencies
			for(const auto& cur : futures) cur.wait();

			// process local task
			apply_tuple<sizeof...(Params)>::on(fun, args);
		}

	};


	namespace {

		template<typename T> struct fun_type_helper;

		template<typename R, typename ... P>
		struct fun_type_helper<R(P...)> {
			typedef std::function<R(P...)> type;
		};

		template<typename C, typename R, typename ... P>
		struct fun_type_helper<R(C::*)(P...)> {
			typedef std::function<R(P...)> type;
		};

		template<typename C, typename R, typename ... P>
		struct fun_type_helper<R(C::*)(P...) const> {
			typedef std::function<R(P...)> type;
		};


		template<typename L, typename F = decltype(&L::operator())>
		struct fun_type : public fun_type_helper<F> {};

	}


	namespace {

		template<typename R, typename ... P>
		Task<R,P...> _task(const std::function<R(P...)>& fun, const P& ... args) {
			return Task<R,P...>(fun, args...);
		}

	}

	// Lambda => Task
	template<typename L, typename F = typename fun_type<L>::type, typename ... Args>
	auto task(const L& l, const Args& ... args) -> decltype(_task(F(l),args...)) {
		return _task(F(l), args...);
	}

	// Function Pointer => Task
	template<typename R, typename ... P>
	Task<R,P...> task(R(*fun)(P...), const P& ... args) {
		return Task<R,P...>(fun, args...);
	}

	// std::function => Task
	template<typename R, typename ... P>
	Task<R,P...> task(const std::function<R(P...)>& fun, const P& ... args) {
		return _task(fun, args...);
	}

	// an empty dummy task
	Task<void> task() {
		return task([](){});
	}

} // utils
} // tasks
