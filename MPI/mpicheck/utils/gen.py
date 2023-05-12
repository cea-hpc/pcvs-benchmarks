import os
from glob import glob
import json

standard_labels = {}
stat = {}

def clean_mpi_name(name):
    lname = name.lower()
    return "MPI_{}".format(lname[4:].capitalize())

def load_def(file):
    ret = []
    with open(file,"r") as f:
        for line in f:
            if not line.startswith("MPI_"):
                continue
            array = line.strip().split(" ", 1)
            funcname = array[0]
            default_tags = set()
            if len(array) > 1:
                default_tags = array[1:]

            yield (clean_mpi_name(funcname), set(default_tags))


for f in sorted(glob("*.dat")):
    std = f.replace(".dat", "")
    print("Processing ... {}".format(std))
    std_tag = "STD:{}".format(std)
    for func, tagset in load_def(f):
        stat.setdefault(std, {'total': 0, 'new': 0})
        if func not in standard_labels:
            standard_labels[func] = set()
            stat[std]['new'] += 1

        standard_labels[func].add(std_tag)
        if tagset:
            standard_labels[func].update(tagset)
        stat[std]['total'] += 1

for f in sorted(glob("./tags/*")):
    cat = os.path.basename(f)
    with open(f, 'r') as fh:
        for line in fh:
            func = line.strip()
            if not line.startswith("MPI_"):
                continue
            standard_labels.setdefault(func, set())
            standard_labels[func].add(cat)

            large_func = "{}_c".format(func)
            if large_func in standard_labels:
                standard_labels[large_func].add(cat)

with open("standard_level.json", "w") as f:
    json.dump(standard_labels, f, default=lambda x: sorted(list(x)))

print("!!!!!!!!!!!!!!!")
print("STATISTICS !!!!")
print("!!!!!!!!!!!!!!!")

tot = 0
for std in sorted(stat.keys()):
    print("{} len {} tot {}".format(std, stat[std]['new'], stat[std]['total']))

