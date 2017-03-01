

//   ------   -----   ------
//  |      | |     | |      |
//  v      | v     | v      |
//  D ----> E ----> F ----> G


typedef struct g_s{
	struct f_s *_g_el;
} g;

typedef struct f_s {
	g *_f_el1;
	struct e_s *_f_el2;
} f;

typedef struct e_s {
	f *_e_el1;
	struct d_s *_e_el2;
} e;

typedef struct d_s {
	e *_d_el;
} d;

int main() {

    d el;

    return 0;
}
