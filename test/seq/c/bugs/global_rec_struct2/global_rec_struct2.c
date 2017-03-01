#include <stdio.h>
typedef struct jpc_mqstate_s {
    struct jpc_mqstate_s *nlps;
} jpc_mqstate_t;

//NOT WORKING GLOBAL 1
//jpc_mqstate_t jpc_mqstates0[3] = { {&jpc_mqstates0[1]} };

//NOT WORKING GLOBAL 2
//jpc_mqstate_t jpc_mqstates1[3] = { {0} };

// working global
jpc_mqstate_t jpc_mqstates_nonInit[3];
jpc_mqstate_t jpc_mqstates0var = {&jpc_mqstates_nonInit[0]};
jpc_mqstate_t jpc_mqstates1var = {0};

//WORKAROUND:
jpc_mqstate_t jpc_mqstates_workaround[3];

int main() {
	{
		//NOT WORKING GLOBAL 1
		//jpc_mqstates0[0];
	}
	{
		//NOT WORKING GLOBAL 2
		//jpc_mqstates1[0];
	}
	{
		// NOT WORKING STATIC 1
		//static jpc_mqstate_t static_jpc_mqstates[3] = { {&static_jpc_mqstates[1]} };
		//static_jpc_mqstates[0];
	}
	{
		// NOT WORKING STATIC 2
		//static jpc_mqstate_t static_x[3] = { {0} };
		//static_x[0];
	}

	{
		// NOT WORKING LOCAL 1
		//jpc_mqstate_t jpc_mqstates_local[3] = { {&jpc_mqstates_local[0]} };
		//jpc_mqstates_local[1];
	}
	{
		// NOT WORKING LOCAL 2
		//jpc_mqstate_t jpc_mqstates_local[3] = { {0} };
		//jpc_mqstates_local[1];
	}

	{
		// working global
		jpc_mqstates_nonInit[0];
		jpc_mqstates0var;
		jpc_mqstates1var;
	}

	{
		// working static
		static jpc_mqstate_t static_xNonInit[3];
		static_xNonInit[0];
	}
	
	{
		//WORKAROUND:
		{
			jpc_mqstate_t temp = { &jpc_mqstates_workaround[0]};
			jpc_mqstates_workaround[0] = temp;
			jpc_mqstates_workaround[1] = temp;
			jpc_mqstates_workaround[2] = temp;
		}
		printf("%d\n", jpc_mqstates_workaround[0].nlps == &jpc_mqstates_workaround[0]);
		printf("%d\n", jpc_mqstates_workaround[1].nlps == &jpc_mqstates_workaround[0]);
		printf("%d\n", jpc_mqstates_workaround[2].nlps == &jpc_mqstates_workaround[0]);
	}

    return 0;
}
