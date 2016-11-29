#include <string.h>

#define NO_ELM(x) (sizeof(x) / sizeof(char *))

const char *get_option_text(int i_option, const char *options[],
                            int option_count);
int get_i_option(const char *option_text, const char *options[],
                        int option_count);

