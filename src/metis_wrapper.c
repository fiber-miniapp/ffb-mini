#include <stdlib.h>
#include "metis.h"

/* assume int = int32_t = fortran's INTEGER */

void metis_wrapper(int *n, int *xadj, int *adjncy, int *nparts, int *part)
{
#ifndef METIS_VER_MAJOR
/* Metis 4.X (or METISLib in ParaMetis 3.X) */

  int edgecut;
  int wgtflag = 0;
  int numflag = 1;  /* fortran-stype numbering */
  int options[5] = { 0, 3, 1, 1, 0 };  /* default values */

  METIS_PartGraphRecursive(n, xadj, adjncy, NULL, NULL, &wgtflag, &numflag,
                           nparts, options, &edgecut, part);

#elif METIS_VER_MAJOR == 5
/* Metis 5.X */

  int edgecut;
  int ncon = 1;
  int options[METIS_NOPTIONS];

  METIS_SetDefaultOptions(options);
  options[METIS_OPTION_NUMBERING] = 1;  /* fortran-stype numbering */

  METIS_PartGraphRecursive(n, &ncon, xadj, adjncy, NULL, NULL, NULL,
                           nparts, NULL, NULL, options, &edgecut, part);

#else
#error unknown Metis version
#endif
}

void METIS_WRAPPER(int *n, int *xadj, int *adjncy, int *nparts, int *part)
{
  metis_wrapper(n, xadj, adjncy, nparts, part);
}

void metis_wrapper_(int *n, int *xadj, int *adjncy, int *nparts, int *part)
{
  metis_wrapper(n, xadj, adjncy, nparts, part);
}

void metis_wrapper__(int *n, int *xadj, int *adjncy, int *nparts, int *part)
{
  metis_wrapper(n, xadj, adjncy, nparts, part);
}
