# Create dictionaries with dependencies of Fortran files on modules
# and include files
import sys
import glob
import os
import cPickle
import re

use_pattern = "^use\s+([a-z][a-z0-9_]*)"
include_pattern = "^include\s+\"(.+)\""

def gendepend(dict_filename):

    try:
        # check if dictionaries are present
        dict_file = open(dict_filename, 'rb')
        dep_dict = cPickle.load(dict_file)
        src_files = set(new_files)
    except:
        # no dictionaries present
        dep_dict = {}
        print("Analyzing dependencies on Fortran modules\n")
        src_files = glob.glob('*.f90')
    else:
        # dictionaries present.
        print("Updating dependencies on Fortran modules")
        for srcfile in src_files:
            sys.stdout.write(srcfile + ' ')
        print('\n')

    update_dep_dict(src_files, dep_dict)
        
    # write dictionary to a file
    dict_file = open(dict_filename, 'wb')
    cPickle.dump(dep_dict, dict_file)
    dict_file.close()

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
        m = re.search(use_pattern, line)
        if m != None:
            # syntax is "use mymodule"
            module_name = m.group(1)
            if module_name != "iso_c_binding":
                # add filename to array
                # skip the intrinsic module "iso_c_binding"
                depend.add(module_name + ".o")
        m = re.search(include_pattern, line)
        if m != None:
            # syntax is "include "mymodule""
            include_name = m.group(1)
            depend.add(os.path.join(include_dir, include_name))
    return depend

# main program
if __name__ == "__main__":
    dict_filename = sys.argv[1]
    include_dir = sys.argv[2]
    gendepend(dict_filename)
