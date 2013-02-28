#include <stdio.h>

struct rec_data_struct_1_s;

typedef struct rec_data_struct_2_s {
        void (*destroy)(struct rec_data_struct_1_s *pxform);
} rec_data_struct_2_t;

typedef struct rec_data_struct_3_s {
        int size;
} rec_data_struct_3_t;

typedef struct rec_data_struct_4_s {
        rec_data_struct_3_t luts[3];
} rec_data_struct_4_t;

typedef struct rec_data_struct_1_s {
        rec_data_struct_2_t *ops;
        union union_1_u {
                rec_data_struct_4_t shapmat;
        } data; 
} rec_data_struct_1_t;

int main()
{
	rec_data_struct_2_t test;

	return 0;
}
