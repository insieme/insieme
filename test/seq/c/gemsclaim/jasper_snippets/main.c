#include <stdio.h>
#include "jpc_t1dec.h"

static int jpc_dec_decodecblk(jpc_dec_tile_t *tile, jpc_dec_tcomp_t *tcomp)
{
	int compno;
	compno = tcomp - tile->tcomps;
    return compno;
}

static int jpc_dec_decodecblk_reversed(jpc_dec_tile_t *tile, jpc_dec_tcomp_t *tcomp)
{
	int compno;
	compno = tile->tcomps - tcomp;
    return compno;
}

int main() {
    jpc_dec_tile_t tile;
	tile.tcomps = NULL;
    jpc_dec_tcomp_t *tcomp = NULL;

    int x = jpc_dec_decodecblk(&tile, tcomp);
	printf("%d == 0 \n",x);
    int y = jpc_dec_decodecblk_reversed(&tile, tcomp);
	printf("%d == 0 \n",y);
	return 0;
}


