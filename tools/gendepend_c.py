# Create dictionaries with dependencies of Fortran files on modules
# and include files

import sys
import glob
import os
import cPickle
import textwrap
import re

# current directory
startdir = os.getcwd()

dep_filename  = sys.argv[1]
dict_filename = "dep_c.pkl"
include_pattern = "^#include\s+\"(.+)\""

def main():

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

    # write dependency file
    f = open(dep_filename, 'w')
    srcs = list(dep_dict.keys())
    srcs.sort()
    for src in srcs:
        rule = os.path.splitext(src)[0] + ".o :"
        nblanks = len(rule)
        depends = list(dep_dict[src])
        depends.sort()
        for depend in depends :
            rule = rule + " " + depend
        rule = (" \\\n" + ' ' * nblanks).join(textwrap.wrap(rule))
        f.write(rule)
        f.write('\n')
    f.close()

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
            print m.groups()
            include_name = m.group(1)
            depend.add(include_name)
    return depend

#call main program
main()
