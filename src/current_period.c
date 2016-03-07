/* Store the current period */

#include <R.h>
#include <stdio.h>
#include "current_period.h"

static int first_year, first_subp, freq;
static char freq_char;

char cur_per[12];

void F77_SUB(set_base_period)(int *y, int *subp, int *frequency) {  
    first_year = *y;
    first_subp = *subp;
    freq = *frequency;
    switch (freq) {
        case 2:  freq_char = 'H';
                 break;
        case 4:  freq_char = 'Q';
                 break;
        case 12: freq_char = 'M';
                 break;
        default: freq_char = '-';
                 break;
    }
}

void F77_SUB(set_current_period)(int *jper) {
   if (freq == 1) {
       sprintf(cur_per, "%d", first_year + *jper);
   } else {
       int nsubp = first_year * freq + first_subp - 1 + *jper - 1;
       int year = nsubp / freq;
       int subp = nsubp % year + 1;
       sprintf(cur_per, "%4d %c%d", year, freq_char, subp);
   }
}
