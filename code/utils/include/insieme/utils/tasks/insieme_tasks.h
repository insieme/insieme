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


#ifndef _GLIBCXX_USE_NANOSLEEP
#define _GLIBCXX_USE_NANOSLEEP
#endif
#include <functional>
#include <iostream>
#include <vector>
#include <tuple>
#include <future>
#include <chrono>
#include <thread>
#include <list>
#include <algorithm>
#include <memory>


namespace insieme {
namespace utils {

	std::mutex glob_mutex;
	#define GLOBAL_LOCK(x)                                                                                                                                     \
		{}
	//{ glob_mutex.lock(); x;  glob_mutex.unlock(); }


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	struct TaskBase;
	typedef std::shared_ptr<TaskBase> TaskPtr;

	struct TaskBase {
		mutable std::vector<TaskPtr> dependencies;

		static int id_counter;
		int id;
		std::mutex running;
		bool done;

		TaskBase() : id(id_counter), done(false) {
			GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " new TASK: " << id << " [" << this << "]" << std::endl);
			id_counter++;
		}

		virtual ~TaskBase(){};

		virtual void operator()() = 0;

		friend class Task;
	};
	int TaskBase::id_counter = 0;


	class Task {
		TaskPtr ptr;

	  public:
		Task() : ptr(nullptr) {}

		Task(TaskPtr p) : ptr(p) {}

		Task(const Task& o) : ptr(o.ptr) {}

		void operator()() {
			if(ptr) { (*ptr)(); }
		}

		Task& operator>>(const Task& b) {
			GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " ARRANGE: " << this->ptr->id << " executes before: " << b.ptr->id << std::endl);
			b.ptr->dependencies.push_back(ptr);
			return *this;
		}
	};


	namespace {

		template <unsigned N>
		struct apply_tuple {
			template <typename F, typename T, typename... Args>
			static void on(const F& fun, const T& tuple, const Args&... args) {
				apply_tuple<N - 1>::on(fun, tuple, std::get<N - 1>(tuple), args...);
			}
		};

		template <>
		struct apply_tuple<0> {
			template <typename F, typename T, typename... Args>
			static void on(const F& fun, const T&, const Args&... args) {
				fun(args...);
			}
		};
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	/**
	 *	its dutty is to get the work done,
	 */
	class TaskManager {
		unsigned numThreads;
		std::list<TaskBase*> submitted;
		std::list<TaskBase*> ran;
		std::mutex lock;

		class Worker {
			TaskManager& mgr;
			std::thread* thread;
			int& done;

		  public:
			Worker(TaskManager& m, int& done) : mgr(m), done(done) {
				// start the thread!
				GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " starting worker" << std::endl);
				thread = new std::thread(*this);
			}
			Worker(Worker&& o) : mgr(o.mgr), thread(o.thread), done(o.done) {}
			Worker(const Worker& o) : mgr(o.mgr), thread(o.thread), done(o.done) {}

			Worker& operator=(const Worker& o) {
				thread = o.thread;
				done = o.done;
				return *this;
			}

			void operator()() {
				GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " START " << std::endl);
				while(!done) {
					while(!mgr.submitted.empty()) {
						auto tsk = mgr.getTask();
						if(tsk) {
							GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " executing: " << tsk->id << std::endl);
							(*tsk)();
						}
					}
					//	GLOBAL_LOCK(std::cerr <<  std::this_thread::get_id() <<  " no more work " << this << " = " << done << std::endl);
				}
				GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " END " << std::endl);
			}

			void finish() {
				GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " signaled to finish " << this << " = " << done << std::endl);
				done = true;
				thread->join();
				delete thread;
			}
		};

		std::vector<Worker> workers;
		// because the spetialization of vector bool, we can not return references, so we need to use chars, and old school 0, 1
		std::vector<int> workFlags;

		////////////////////////////////////////////////
		//
		////////////////////////////////////////////////

		TaskManager(unsigned nth) : numThreads(nth), workers(), workFlags(numThreads, false) {
			GLOBAL_LOCK(std::cerr << std::this_thread::get_id() << " start " << numThreads << " workers " << std::endl);
			for(unsigned i = 0; i < numThreads; ++i) {
				workers.push_back(Worker(*this, workFlags[i]));
			}
		}

		TaskBase* getTask() {
			TaskBase* t = nullptr;
			lock.lock();
			if(!submitted.empty()) {
				t = submitted.front();
				submitted.pop_front();
				ran.insert(ran.begin(), t);
			}
			lock.unlock();
			return t;
		}

		////////////////////////////////////////////////
		// Static interface
		////////////////////////////////////////////////
	  public:
		static void addTask(TaskBase* t) {
			GLOBAL_LOCK(std::cerr << " add task: " << t->id << " [" << t << "]" << std::endl);
			TaskManager& mgr = getInstance();
			mgr.lock.lock();
			mgr.submitted.push_back(t);
			mgr.lock.unlock();
		}

		static void wait(TaskBase* t) {
			TaskManager& mgr = getInstance();

			GLOBAL_LOCK(std::cerr << " wait task: " << t->id << " [" << t << "]" << std::endl);
			// once the task is not waiting anymore, it might be running (by someone else right now) or it might be that we ran it before
			while(!t->done) {
				auto tsk = mgr.getTask();
				if(tsk) {
					GLOBAL_LOCK(std::cerr << "  activeWait2: " << tsk->id << std::endl);
					(*tsk)();
				}
			}
		}
		static void configure(unsigned nth) {
			getInstance(nth - 1);
		}

		static unsigned getNumWorkers() {
			return getInstance().numThreads + 1;
		}

		static void finalize() {
			TaskManager& mgr = getInstance();
			// wait for everithing not finnished, the problem is with the pointers, if we leave the scope. non finished works will
			// be deleted and well have pointers to dirty memory
			for(auto& f : mgr.workFlags) {
				f = false;
			}
		}

		static TaskManager& getInstance(int nth = 7) {
			static TaskManager inst(nth);
			return inst;
		}

		friend struct TaskBase;
	};

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template <typename R, typename... Params>
	class TaskHelper : public TaskBase {
		typedef std::function<R(Params...)> fun_t;

		fun_t fun;

		std::tuple<Params...> args;

		mutable bool owner;

	  public:
		TaskHelper(const fun_t& fun, const Params&... args) : fun(fun), args(args...), owner(true) {}


		TaskHelper(const TaskHelper<R, Params...>& o) : fun(o.fun), args(args), owner(true) {
			o.owner = false;
		}

		TaskHelper<R, Params...> operator=(const TaskHelper<R, Params...>& o) = delete;

		//		~TaskHelper(){
		//			if (owner) (*this)();
		//		}

		//////////////////////////////////////////
		// operation
		void operator()() {
			// make sure it hasn't been processed before and is only processed once (DAG)
			if(done) { return; }

			// avoid concurrent executions
			if(!running.try_lock()) { return; }

			// process dependencies concurrently
			for(auto cur : dependencies) {
				TaskManager::addTask(cur.get());
			}

			// wait for dependencies
			for(auto cur : dependencies) {
				TaskManager::wait(cur.get());
			}

			// process local task
			apply_tuple<sizeof...(Params)>::on(fun, args);

			done = true;
			running.unlock();
		}
	};


	namespace {

		template <typename T>
		struct fun_type_helper;

		template <typename R, typename... P>
		struct fun_type_helper<R(P...)> {
			typedef std::function<R(P...)> type;
		};

		template <typename C, typename R, typename... P>
		struct fun_type_helper<R (C::*)(P...)> {
			typedef std::function<R(P...)> type;
		};

		template <typename C, typename R, typename... P>
		struct fun_type_helper<R (C::*)(P...) const> {
			typedef std::function<R(P...)> type;
		};


		template <typename L, typename F = decltype(&L::operator())>
		struct fun_type : public fun_type_helper<F> {};
	}


	namespace {

		template <typename R, typename... P>
		// TaskHelper<R,P...> _task(const std::function<R(P...)>& fun, const P& ... args) {
		Task _task(const std::function<R(P...)>& fun, const P&... args) {
			return Task(std::make_shared<TaskHelper<R, P...>>(fun, args...));
		}
	}

	// Lambda => Task
	template <typename L, typename F = typename fun_type<L>::type, typename... Args>
	// auto task(const L& l, const Args& ... args) -> decltype(_task(F(l),args...)) {
	Task task(const L& l, const Args&... args) {
		return _task(F(l), args...);
	}

	// Function Pointer => Task
	template <typename R, typename... P>
	// TaskHelpe<R,P...> task(R(*fun)(P...), const P& ... args) {
	Task task(R (*fun)(P...), const P&... args) {
		return Task(std::make_shared<TaskHelper<R, P...>>(fun, args...));
	}

	// std::function => Task
	template <typename R, typename... P>
	// Task<R,P...> task(const std::function<R(P...)>& fun, const P& ... args) {
	Task task(const std::function<R(P...)>& fun, const P&... args) {
		return _task(fun, args...);
	}

	// an empty dummy task
	Task task() {
		return task([]() {});
	}

} // utils
} // tasks
