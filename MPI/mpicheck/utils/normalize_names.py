from glob import glob

for f in glob("*.dat"):
    l = list()
    with open(f, "r") as fhi, open("{}.convert".format(f), "w") as fho:
        for func in fhi:
            func = func.strip()
            fho.write("MPI_{}\n".format(func[4:].capitalize()))

