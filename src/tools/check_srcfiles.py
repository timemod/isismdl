#
# This python script checks if source files (files in src/xp, src/libsl or
# src/libf90) have been deleted. If so, the object files, library files and
# dependency dictionary files are deleted.
#
import glob
import os
import cPickle
import sys

import gendepend_f90
import gendepend_c 
import write_makedeps

# extension for object files:
OBJ_EXT = 'o'
MOD_EXT = 'mod'
FOR_EXT = 'f90'
LIB_EXT = 'dll'
f90_pkl = "dep_f90.pkl"
c_pkl   = "dep_c.pkl"
f90_dep = "makedeps_f90"
c_dep   = "makedeps_c"

def remove_file(filename):
    if os.path.exists(filename):
        print("Deleting file " + filename)
        os.remove(filename)
        return True
    else:
        return False

'''get_name returns the name of a file without path and extension '''
def get_name(filename):
    return os.path.splitext(os.path.basename(filename))[0]

''' checks .mod and object files, deletes obsolete files and returns the
    number of deleted files'''
def check_compiled_files(ext, src_names):
    delete_count = 0
    compiled_files = glob.glob("*." + ext)
    for compiled_file in compiled_files:
        if not get_name(compiled_file) in src_names:
            if remove_file(compiled_file):
                delete_count = delete_count + 1
    return delete_count

'''Checks dictionary. Returns true if the dictionary contains a source file that
no longer exists.
'''
def dictionary_is_obsolete(dict_filename, src_names):
    dict_file = open(dict_filename, 'rb')
    dep_dict = cPickle.load(dict_file)
    dict_file.close()
    src_files = list(dep_dict.keys())
    for src_file in src_files:
        if not get_name(src_file) in src_names:
            return True
    return False


dep_dir = sys.argv[1]
f90_pkl = os.path.join(dep_dir, f90_pkl)
c_pkl = os.path.join(dep_dir, c_pkl)
f90_dep = os.path.join(dep_dir, f90_dep)
c_dep = os.path.join(dep_dir, c_dep)

# main script
f90_names = [get_name(src_file) for src_file in glob.glob("*.f90")]
c_names   = [get_name(src_file) for src_file in glob.glob("*.c")]
src_names = f90_names + c_names

# module files
check_compiled_files(MOD_EXT, f90_names)

# object files and library
if check_compiled_files(OBJ_EXT, src_names) > 0:
    libfile = glob.glob("*." + LIB_EXT)
    if len(libfile) > 0:
        remove_file(libfile[0])

# dictionaries
if dictionary_is_obsolete(f90_pkl, f90_names):
    gendepend_f90.gendepend(f90_pkl)
    write_makedeps.write_makedeps(f90_pkl, f90_dep)

if dictionary_is_obsolete(c_pkl, c_names):
    gendepend_f90.gendepend(c_pkl)
    write_makedeps.write_makedeps(c_pkl, c_dep)
