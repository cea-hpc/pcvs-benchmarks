from mpiiface import MPI_Interface, MPI_Standard_meta
import argparse


parser = argparse.ArgumentParser(description='MPI Interface Generation tool')
parser.add_argument('datafile', default="./prepass.dat", metavar="FILE", type=argparse.FileType('r'), nargs="?", help='Pre-generated JSON file extracted from documentation')
parser.add_argument('--standard', default="4.0.0", dest="mpi_version", metavar="VERSION", type=str, nargs="?", help='MPI version (defaults to 4.x)')

args = parser.parse_args()
a = MPI_Interface(args.datafile, MPI_Standard_meta(lang="c", fprefix="", mpi_version=args.mpi_version))


def gen_c_iface(f):
	print("\n")
	print("/*{}*/".format(f.name()))
	print(f.doxygen())
	print("{};".format(f.proto()))
	print("{};".format(f.proto(prefix="P")))



def is_part_of_bindings(f):
    return f.isbindings() and \
        f.isc() and \
		not f.iscallback() and \
        not f.isf08conv()


a.forall(gen_c_iface, is_part_of_bindings)
