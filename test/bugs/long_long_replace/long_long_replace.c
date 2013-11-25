#include <stdint.h>


typedef struct {

	uint_fast16_t numcomps;

} jpc_cstate_t;


static int jpc_qcc_getparms(jpc_cstate_t *cstate) {
	if (cstate->numcomps <= 256) {
		return 0;
	}

	return 1;
}

int main() {
	jpc_cstate_t cstate;

	jpc_qcc_getparms(&cstate);
	return 0;
}
