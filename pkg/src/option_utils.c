#include <string.h>
#include "option_utils.h"

const char *get_option_text(int i_option, const char *options[],
                                   int option_count) {
    static const char *UNKNOWN = "???";
    if (i_option < 1 || i_option > option_count) {
       return UNKNOWN;
    } else {
        return options[i_option - 1];
    }
} 

int get_i_option(const char *option_text, const char *options[],
                        int option_count) {
    int i;
    for (i = 0; i < option_count; i++) {
        if (strcmp(option_text, options[i]) == 0) {
            return i + 1;
        }
    }
    return option_count;
}
