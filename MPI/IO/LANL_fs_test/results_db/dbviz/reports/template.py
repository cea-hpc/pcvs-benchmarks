#! /usr/bin/env python

import dbviz

title   = "Scaling on "
summary = """Here is my summary.
             Here is more summary."""

report = dbviz.Report(title,summary)
report.config["GraphTitle"] = ""
report.config['Where'] = " " \
report.config['XAxis'] = ""
report.config['XAxisTitle'] = ""
report.config['MaxY'] = "1200"
report.config['Compare'] = "left(target,27)"

for y in ( '' ):
  for io in ( 'read', 'write' ):
    report.config['YAxis'] = "var/%s_%s" % ( io, y )
    report.add_graph()
path = report.compile( )
print "%d graphs combined in %s/%s" % (len(report.graphs),report.outdir,path)
