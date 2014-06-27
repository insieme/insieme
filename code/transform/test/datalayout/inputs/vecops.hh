// standard NVIDIA utilities and system includes
//#include <oclUtils.h>

#define PATH_MAX 255

typedef float dtype;

struct svm_node
{
	int index;
	dtype value;
};


struct big {
	dtype value;
	dtype fillup[127];
};

