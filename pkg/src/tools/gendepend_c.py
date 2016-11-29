# Create dictionaries with dependencies of C files on header files
import sys
import glob
import os
import cPickle
import re

include_pattern = "^#include\s+\"(.+)\""

def gendepend(dict_filename):

    try:
        # check if dictionaries are present
        dict_file = open(dict_filename, 'rb')
        dep_dict = cPickle.load(dict_file)
        src_files = set(new_files)
    except:
        # no dictionaries present
        dep_dict = {}
        print("Analyzing dependencies of c files on header files\n")
        src_files = glob.glob('*.c')
    else:
        # dictionaries present.
        print("Updating dependencies of C files on header files")
        for srcfile in src_files:
            sys.stdout.write(srcfile + ' ')
        print('\n')

    update_dep_dict(src_files, dep_dict)
        
    # write dictionary to a file
    dict_file = open(dict_filename, 'wb')
    cPickle.dump(dep_dict, dict_file)
    dict_file.close()

    return None

def update_dep_dict(src_files, dep_dict):
    # update dependency dictionaries dep_dict
    modified = False
    # loop over sourcefiles
    for srcfile in src_files :
        # get list of dependencies for scrfile
        depend = read_depend(srcfile)
        if len(depend) > 0:
            dep_dict[srcfile] = depend

def read_depend(srcfile) :
    # read dependencies of file srcfile on modules
    depend = set()
    for line in open(srcfile) :
        line = line.strip() # remove trailing blancs
        line = line.lower()
        m = re.search(include_pattern, line)
        if m != None:
            # syntax is "include "mymodule""
            include_name = m.group(1)
            depend.add(include_name)
    return depend

# main program
if __name__ == "__main__":
    dict_filename = sys.argv[1]
    gendepend(dict_filename)
