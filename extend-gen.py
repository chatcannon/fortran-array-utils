#!/usr/bin/python2

"""
Script to drive templated generation of the extendable arrays module

Copyright (C) 2013 Chris Kerr
You may use this software under the terms of the GNU General Public License (GPL)
version 3 or, at your option, any later version
"""

import sys
from Cheetah import Template

Nmax = 3
types = [("INTEGER", "int"),
         ("REAL", "real"),
         ("DOUBLE PRECISION", "dble")]

descriptors = []
for N in range(1, Nmax+1):
  for typepair in types:
    typeN_descriptor = dict()
    typeN_descriptor['N'] = N
    typeN_descriptor['longtype'] = typepair[0]
    typeN_descriptor['shorttype'] = typepair[1]
    typeN_descriptor['typeN'] = "%s_%d" % (typepair[1],N)
    typeN_descriptor['dNs'] = ["d%d" % i for i in range(1, N+1)]
    typeN_descriptor['colons'] = [":" for i in range(1, N+1)]
    descriptors.append(typeN_descriptor)
    
descriptor_tuple_order = ('N', 'longtype', 'shorttype', 'typeN', 'dNs', 'colons')

fortran_template = Template.Template(file="extend.tmpl.F90")
fortran_template.descriptor_tuples = [tuple([d[hdr] for hdr in descriptor_tuple_order]) for d in descriptors]
fortran_template.all_typeN = [d['typeN'] for d in descriptors]

with open(sys.argv[1], 'w') as fortran_file:
  fortran_file.write(str(fortran_template))