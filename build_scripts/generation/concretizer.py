#!/usr/bin/env python
import sys
import spack.spec as s
import spack.modules.tcl as t
for package in sys.argv[1:]:
    tmp = s.Spec(package)
    tmp.concretize()
    print ((t.TclModulefileWriter(tmp)).layout.use_name)
    print tmp.format("${prefix}")
