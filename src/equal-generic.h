#ifndef VCTRS_EQUAL_GENERIC_H
#define VCTRS_EQUAL_GENERIC_H


void equal_fill(SEXPTYPE type,
                const void* p_x,
                const void* p_y,
                r_ssize size,
                bool na_equal,
                int* p_out);


#endif
