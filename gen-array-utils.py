#!/usr/bin/python2

"""
Script to drive templated generation of the extendable arrays module

Copyright (C) 2013 Chris Kerr

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the author nor the names of any
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

import sys
from Cheetah import Template

Nmax = 3
types = [("INTEGER", "int"),
         ("CHARACTER(LEN=*)", "char"),
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

fortran_template = Template.Template(file=sys.argv[1])
fortran_template.descriptor_tuples = [tuple([d[hdr] for hdr in descriptor_tuple_order]) for d in descriptors]
fortran_template.all_typeN = [d['typeN'] for d in descriptors]

with open(sys.argv[2], 'w') as fortran_file:
  fortran_file.write(str(fortran_template))