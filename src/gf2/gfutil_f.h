#ifndef GFUTIL_F_H
#define GFUTIL_F_H

#if F2C001
#define open_f_   OPEN_F
#define close_f_  CLOSE_F
#define rw_c1_08_ RW_C1_08
#define rw_c1_30_ RW_C1_30
#define rw_c1_60_ RW_C1_60
#define rw_i1_    RW_I1
#define rw_i2_    RW_I2
#define rw_i3_    RW_I3
#define rw_in_    RW_IN
#define rw_f1_    RW_F1
#define rw_f2_    RW_F2
#define rw_f3_    RW_F3
#define rw_fn_    RW_FN
#define rw_d1_    RW_D1
#define rw_d2_    RW_D2
#define rw_d3_    RW_D3
#define rw_dn_    RW_DN
#define printstd_ PRTSTD
#endif

void open_f_(int *, int *, char *, int *);
void close_f_(int *, int *);
void rw_c1_08_(int *, int *, int *, const char *, int *);
void rw_c1_30_(int *, int *, int *, const char *, int *);
void rw_c1_60_(int *, int *, int *, const char *, int *);
void rw_i1_(int *, int *, int *, int *, int *, int *, int *);
void rw_i2_(int *, int *, int *, int *, int *, int *, int *, int *);
void rw_i3_(int *, int *, int *, int *, int *, int *, int *, int *, int *);
void rw_in_(int *, int *, int *, int *, int *, int *, int *, int *, int *);
void rw_f1_(int *, int *, int *, int *, int *, float *, int *);
void rw_f2_(int *, int *, int *, int *, int *, float *, float *, int *);
void rw_f3_(int *, int *, int *, int *, int *, float *, float *, float *, int *);
void rw_fn_(int *, int *, int *, int *, int *, int *, int *, float *, int *);
void rw_d1_(int *, int *, int *, int *, int *, double *, int *);
void rw_d2_(int *, int *, int *, int *, int *, double *, double *, int *);
void rw_d3_(int *, int *, int *, int *, int *, double *, double *, double *, int *);
void rw_dn_(int *, int *, int *, int *, int *, int *, int *, double *, int *);
void prtstd_(int *, char *);

#endif /* GFUTIL_F_H */
