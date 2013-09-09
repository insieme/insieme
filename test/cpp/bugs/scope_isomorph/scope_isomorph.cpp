


struct obj{
	int memb;
};

int main (){

	{
		typedef struct obj obja;
		obja a;
	}

	{
		typedef struct obj obja;
		obja a;
	}


	return 0;
}
