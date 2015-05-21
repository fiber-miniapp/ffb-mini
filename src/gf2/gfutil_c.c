#include <stdio.h>
#include <string.h>
#include "gfutil_f.h"
#include "gfutil_c.h"
#include "gfc.h"

void print_std(int *IOUT,char *p){
  int i;
  for(i=strlen(p);i<=LOGLENGTH;i++)p[i]= ' ';
  prtstd_(IOUT,p);
  for(i=0        ;i<=LOGLENGTH;i++)p[i]= ' ';
}


/***************************************************/
/* program to convert endian                       */
/* *a1 (in) : pointer to the date to be converted  */
/* num (in) : byte number                          */  
/*            4 for int and float                  */
/*            8 for double                         */
/* a2 (tmp) : buffer                               */
/***************************************************/       
void conv(char *a1,int num)
{
/*#define DEBUG_conv*/

  int n,n1,n2;
  char a2[8];

  for(n=0;n<num;n++){
   *(a2+n)=*(a1+n);
  }

  for(n=0;n<num;n++){
    n1=n;
    n2=num-1-n;
    *(a1+n2) = *(a2+n1);
  }

#ifdef DEBUG_conv
  printf("Before:");
  for(n=0;n<num;n++){printf("%x|",*(a2+n));} printf("\n");
  printf("After :");
  for(n=0;n<num;n++){printf("%x|",*(a1+n));} printf("\n");
#endif

}

int strcmp_n(int num, const char *c1 ,const char *c2){
 int i;
 for(i=0;i<num;i++){
 if(*(c1+i)!=*(c2+i)) {return(-1);}
 }
 return(0);
}
