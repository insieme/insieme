#include "jpc_t1dec.h"

static int jpc_dec_decodecblk(jpc_dec_tile_t *tile, jpc_dec_tcomp_t *tcomp)
{
	int compno;

	compno = tcomp - tile->tcomps;

    return compno;
}

int main() {
    jpc_dec_tile_t *tile;
    jpc_dec_tcomp_t *tcomp;

    return jpc_dec_decodecblk(tile, tcomp);
}


