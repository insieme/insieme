


namespace toIntercept{

	class mutex{

		mutex(const mutex& ){}
		mutex& operator=(const mutex& ){ return *this;}

	public: 
		
		mutex (){
		}

		void lock(){ }
		void unlock(){ }
	};


}
