import json

from mpiiface import MPI_Interface, MPI_Standard_meta
with open("./prepass.dat", "r") as f:
	a = MPI_Interface(f, MPI_Standard_meta(lang="c", fprefix=""))

the_json = {}

def gen_c_iface(f):
	params = [ [p.type_full_c(), p.name()] for p in f.params()]
	the_json[f.name()] = params



def is_part_of_bindings(f):
    return f.isbindings() and \
        f.isc() and \
		not f.iscallback() and \
        not f.isf08conv()


a.forall(gen_c_iface, is_part_of_bindings)


print(json.dumps(the_json, indent=4, sort_keys=True))
