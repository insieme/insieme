#include <stdio.h>
#include <stdlib.h>

typedef int jpc_ms_t;
typedef int jpc_cstate_t;
typedef int jas_stream_t;

typedef struct jpc_msops_s {

        /* Destroy the marker segment parameters. */
        void (*destroyparms)(jpc_ms_t *ms);

        /* Get the marker segment parameters from a stream. */
        int (*getparms)(jpc_ms_t *ms, jpc_cstate_t *cstate, jas_stream_t *in);

        /* Put the marker segment parameters to a stream. */
        int (*putparms)(jpc_ms_t *ms, jpc_cstate_t *cstate, jas_stream_t *out);

        /* Dump the marker segment parameters (for debugging). */
        int (*dumpparms)(jpc_ms_t *ms, FILE *out);

} jpc_msops_t;

typedef struct {
        jpc_msops_t ops;
} jpc_mstabent_t;

static jpc_mstabent_t jpc_mstab[] = {
        {{NULL, NULL, NULL, NULL}}
};


int main()
{
	jpc_mstabent_t a = {{NULL, NULL, NULL, NULL}};
	a.ops.destroyparms == NULL;

	jpc_mstab[0].ops.destroyparms == NULL;

	jpc_mstabent_t b[1] = { {{NULL, NULL, NULL, NULL}} };
	b[0].ops.destroyparms == NULL;

	return 0;
}
