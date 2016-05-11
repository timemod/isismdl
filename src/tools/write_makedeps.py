# Write makedeps file with dependencies

import sys
import os
import cPickle
import textwrap

def write_makedeps(dict_filename, dep_filename):

    print "writing dependency file " + dep_filename
    # read dictionary
    dict_file = open(dict_filename, 'rb')
    dep_dict = cPickle.load(dict_file)
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

# main program
if __name__ == "__main__":
    dict_filename = sys.argv[1]
    dep_filename  = sys.argv[2]
    write_makedeps(dict_filename, dep_filename)
