import os
import json

bindings = {}

with open("./standard_level.json","r") as f:
   bindings = json.load(f)

stds = ["1.0","1.1","1.3","2.0","2.1","2.2","3.0", "3.1", "4.0"]

print("""
<html>

<head>
<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha3/dist/css/bootstrap.min.css" rel="stylesheet">
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha3/dist/js/bootstrap.min.js"></script>
<!--<script type="text/javascript">
$(document).ready(function(){
    $('#the_table').DataTable({
        paging: false,
        fixedHeader: true,
        order: [[0, 'asc']],
        columnDefs: [
           {"className": "dt-center", "targets": "standards"},
        ]
    });
});
</script>-->

<style>
div.container {
    width: 80%;
    margin-right: auto;
    margin-left: auto;
}

h1 {
   text-align: center;
}

.theader {
      position: sticky;
      top: 0;
}

pre {
   margin-top: 4px;
   margin-bottom: 4px;
}

#deprecated {
   color: orange;
   font-size: x-large;
}

#present {
   color: green;
   font-size: x-large;
}

#notpresent {
   color: red;
   font-size: x-large;
}

#notdefined{
   color: #555;
   font-size: small;
}

</style>


<title>Table of MPI Functions</title>
</head>

<body data-bs-theme="dark">

<h1>MPI Function Table</h1>

<div class="container">
<table class="table table-striped table-dark">
  <thead class="theader thead-dark">
    <th class="theader">MPI Functions</th>
""")

for s in stds:
   print("    <th class='standards'>{}</th>".format(s))

print("""
</thead>
<tbody>""")

funcs = [x for x in bindings.keys()]
funcs.sort()

def has_dep(f):
   dep = [x for x in bindings[f] if x.startswith("DEPBY")]
   if len(dep):
      return float(dep[0].split(":")[1])
   return None

def was_present_in_past(f, std):
   vers = [x for x in bindings[f] if x.startswith("STD")]
   vers = [float(x.split(":")[1]) for x in vers]
   vers.sort()
   fstd = float(std)
   for v in vers:
      if v < fstd:
         return True
   return False

def get_symbol_for_std(f, std):
   std_int_val = float(std)

   if "STD:{}".format(std) not in bindings[f]:
      if was_present_in_past(f, std):
         return "<div id='notpresent'>&#10006;</div>".format(std)
      else:
         return "<div id='notdefined'>N/A</div>".format(std)

   deprecated_txt = "<div id='deprecated'>&#9888;</div>".format(std)
   dep = has_dep(f)
   if dep:
      if dep < std_int_val:
         return deprecated_txt
   if "DEPBY:{}".format(std) in bindings[f]:
      return deprecated_txt

   return "<div id='present'>&#10004;</div>".format(std)



for f in funcs:
   print("""
<tr>
    <td><pre>{}</pre></td>
""".format(f))
   for s in stds:
      print("    <td>{}</td>".format(get_symbol_for_std(f, s)))







print("""
</tbody>
</table>
</div>
</body>
""")
