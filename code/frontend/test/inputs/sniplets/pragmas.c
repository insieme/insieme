
#define FAKE(x) \
	{ x++; }


int main (){

	#pragma test "first"
	int x;

#pragma test "macro"
	FAKE(x);

#pragma test "solo"
	

}
