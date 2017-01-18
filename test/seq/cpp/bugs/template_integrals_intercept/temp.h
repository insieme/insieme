extern int global;
namespace NS {
	template <typename T, int N, typename R>
	R f ( T val){

		R res = 0;
		for (int i=0; i<N; i++){
			res += val;
		}
		return res;
		//return res + global;

	}
}
