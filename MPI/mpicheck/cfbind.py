from mpiiface import MPI_Interface, MPI_Standard_meta

with open("./prepass.dat", "r") as f:
	a = MPI_Interface(f, MPI_Standard_meta(lang="fbind", fprefix=""))

# First Print Conversion Functions
print("""
#include <mpc_mpi.h>
#include <sctk_alloc.h>

#include "mpc_fortran_helpers.h"

static inline char * char_fortran_to_c(char *buf, int size, char **free_ptr)
{
        char *   tmp;
        long int i;

        tmp = sctk_malloc(size + 1);
        assume(tmp != NULL);
        *free_ptr = tmp;

        for(i = 0; i < size; i++)
        {
                tmp[i] = buf[i];
        }
        tmp[i] = '\\0';

        /* Trim */

        while(*tmp == ' ')
        {
                tmp++;
        }

        size_t len = strlen(tmp);

        char *begin = tmp;

        while( (tmp[len - 1] == ' ') && (&tmp[len] != begin) )
        {
                tmp[len - 1] = '\\0';
                len--;
        }

        return tmp;
}

static inline void char_c_to_fortran(char *buf, int size)
{
        size_t i;

        for(i = strlen(buf); i < size; i++)
        {
                buf[i] = ' ';
        }
}

#if defined(USE_CHAR_MIXED)
        #define CHAR_END(thename)
        #define CHAR_MIXED(thename) long int thename,
#else
        #define CHAR_END(thename) ,long int thename
        #define CHAR_MIXED(thename)
#endif


static inline int buffer_is_bottom(void * buffer)
{
    return( (buffer == *mpi_predef_bottom()) ||
            (buffer == *mpi_predef08_bottom()) );
}

static inline int buffer_is_mpiinplace(void * buffer)
{
    return( (buffer == *mpi_predef_inplace()) ||
            (buffer == *mpi_predef08_inplace()) );
}

""")

handle_converter = {"COMMUNICATOR": "PMPI_Comm_f2c",
                    "INFO": "PMPI_Info_f2c",
                    "REQUEST": "PMPI_Request_f2c",
                    "OPERATION": "PMPI_Op_f2c",
                    "GROUP": "PMPI_Group_f2c",
                    "WINDOW": "PMPI_Win_f2c",
                    "FILE": "PMPI_File_f2c",
                    "ERRHANDLER": "PMPI_Errhandler_f2c",
                    "COMMUNICATOR": "PMPI_Comm_f2c",
                    "DATATYPE": "PMPI_Type_f2c"}


def get_conv_f2c(kind):
    return handle_converter[kind]

def get_conv_c2f(kind):
    conv = handle_converter[kind]
    if not conv:
        return None
    return conv.replace("f2c", "c2f")

def handle_convert_in_array(p, f):
        ret = ""
        rename_list = {}

        name = p.name()
        length = "*" + p.length()

        if p.array_length():
                ret += "ERRROR " + name

        if f.name().lower().find("alltoallw") != -1:
            length = "alltoallwlen"

        ret += """
int incnt_{0} = 0;
{1} *c_{0} = NULL;

c_{0} = ({1}*) sctk_malloc(sizeof({1}) * {2});

for(incnt_{0} = 0; incnt_{0} < {2} ; incnt_{0}++)
        c_{0}[incnt_{0}] = {3}({0}[incnt_{0}]);
        """.format(name, p.type_c(noconst=True), length, get_conv_f2c(p.kind()) )

        rename_list[name] = "c_{0}".format(name)

        return ret, rename_list



def handle_convert_in(p, f):
    ret = ""
    rename_list = {}

    if p.kind() in handle_converter:
        if p.length():
                return handle_convert_in_array(p, f)

        ret += "{0} c_{1} = {2}(*{1});\n".format(p.type_c(noconst=True),
                                                 p.name(), get_conv_f2c(p.kind()) )
        rename_list[p.name()] = "c_{}".format(p.name())
    return ret, rename_list


def handle_convert_out_forward_declare(p, f):
    ret = ""
    rename_list = {}

    did_process = 0

    if(p.length()):
        # OUTTYPE has a length
        if(p.length() != "*" and p.array_length() == False):
            # Only in mpi_type_get_contents
            ret += "{0} * c_{1} = ({0}*)sctk_malloc(sizeof({0}) * *{2});\n".format(p.type_c(noconst=True), p.name(), p.length())
            did_process = 1
        else:
            #OUTTYPE HAS NO LEN (dynamic in call)
            pass
    else:
        # Outype has no length
        # Output types are references
        p.setfbindgetref()
        ret += "{0} c_{1};\n".format(p.type_c(noconst=True), p.name())
        did_process = 1

    if did_process:
        rename_list[p.name()] = "c_{0}".format(p.name())
    else:
        ret += "/* OUT ARRAY {} not manipulated */\n".format(p.name())

    return ret, rename_list

def handle_convert_out(p, f, rename_list):
    ret = ""

    name = p.name()

    if name in rename_list:
        name = rename_list[name]
    else:
        # Case was not handled
        return

    if(p.length()):
        # OUTTYPE has a length
        if(p.length() != "*"):
            if p.array_length() == False:
               # Lenght is determined and a pointer
               length = "*" + p.length()
               ret += """
int outcnt_{0} = 0;

for(outcnt_{0} = 0; outcnt_{0} < {2} ; outcnt_{0}++)
        {0}[outcnt_{0}] = {3}({1}[outcnt_{0}]);

""".format(p.name(), name, length, get_conv_c2f(p.kind()) )
            else:
                ret += "/* ERROR array len for {} */\n".format(p.name())
            # Only in mpi_type_get_contents
            pass
        else:
            #OUTTYPE HAS NO LEN (dynamic in call)
            pass
    else:
        # Outype has no length
        # Output types are references
        ret += "*{0} = {1}({2});\n".format(p.name(), get_conv_c2f( p.kind() ), name)

    return ret


def parameter_in_conversion(f):
    ret = ""
    rename_list = {}



    # In the alltoallw family length is comm_size
    # so we need a special handing for it
    print("/* {} */".format(f.name()))
    if f.name().lower().find("alltoallw") != -1:
            comms = f.get_param_by_kind("COMMUNICATOR")
            if len(comms) == 1:
                    # We have a single comm
                    ret += "int alltoallwlen = 0;\n"
                    ret += "PMPI_Comm_size({}, &alltoallwlen);\n".format(comms[0].name())



    for p in f.parameters:
        #
        # String Conversion
        #
        if p.kind() == "STRING" and p.isin():
            # Convert from fortran
            name = p.name()
            ret += "char *tmp_{0} = NULL, *ptr_{0} = NULL;\n".format(name)
            rename_list[p.name()] = "tmp_{}".format(name)
            ret += "tmp_{0} = char_fortran_to_c((char *){0}, size_{0}, &ptr_{0});\n".format(
                name)
        #
        # Buffer conversion to MPI_BOTTOM
        #
        if p.kind() == "BUFFER" and p.isin():
            # Add a check for MPI_BOTTOM
            ret += "if( buffer_is_bottom((void *){0}) )\n\t{0} = MPI_BOTTOM;\n".format(
                p.name())

        #
        # MPI In place
        #
        if p.kind() == "BUFFER" and p.isin() and f.iscollective() and f.number_of_buffer_params() >= 2:
            ret += "if( buffer_is_mpiinplace((void *){0}) )\n\t{0} = MPI_IN_PLACE;\n".format(
                p.name())

        if p.ishandle():
            if p.kind() == "STATUS":
                pass  # Statusses are ignored as forwarded to Fortran as struct
            elif p.isin():
                hret, hrename = handle_convert_in(p, f)
                ret += hret
                rename_list.update(hrename)
                p.setfbindnoderef()
                # If param is a pointer we need to pass the local conversion
                # by reference to the C function
                if p.type_c_is_pointer():
                    p.setfbindgetref()
            elif p.intent() == "out":#Make sure not to duplicate inout
                hfret, hfrename = handle_convert_out_forward_declare(p, f)
                ret += hfret
                rename_list.update(hfrename)

    return ret, rename_list



def parameter_out_conversion(f, rename):
    ret = ""

    for p in f.parameters:
        name = p.name()
        if name in rename:
            name = rename[name]
        if p.kind() == "STRING" and p.isout():
            ret += "char_c_to_fortran({0},size_{1});\n".format(name, p.name())
        if p.ishandle() and p.kind() != "STATUS" and p.isout():
            ret += handle_convert_out(p, f, rename)

    # Ensure freing is done last
    for p in f.parameters:
        if p.kind() == "STRING" and p.isin():
            ret += "sctk_free(ptr_{0});\n".format(p.name())
        if p.kind() in handle_converter and p.length() and p.name() in rename:
            ret += "sctk_free({0});\n".format(rename[p.name()])


    return ret


def gen_fortran_iface(f):
    # MPI Sizeof is handled separately
    if f.name() == "MPI_Sizeof":
        return


    for p in f.parameters:
        #
        # Handle conversion all MPI Handles are Fint
        #
        if p.ishandle() and not p.kind() == "STATUS":
            p.set_extern_fbind_type("MPI_Fint")

    param_save = f.parameters
    if f.isinit():
        # In Fortran MPI Init only takes ierror
        f.parameters = f.parameters[2:]

    # The _ version
    print(f.proto(suffix="_", lowername=True))
    print("{")

    if f.isinit():
        # In Fortran MPI Init only takes ierror
        f.parameters = param_save
        print("int *argc = NULL;")
        print("char ***argv = NULL;")


    if f.name() != "MPI_F_sync_reg":
        # sync_reg has an empty body as it is just a way
        # to flush registers in a fortran program
        ret, rename = parameter_in_conversion(f)
        print(ret)
        print(f.gen_call(rename=rename))
        ret = parameter_out_conversion(f, rename)
        print(ret)
        print(f.gen_return())

    print("}")

    # The __ version
    print(f.proto(suffix="__", lowername=True))
    print("{")
    fname = f.name().lower() + "_"
    params = ", ".join([ x.name() for x in f.params()])

    if fname != "mpi_wtime_":
        print("\t{}({}, ierror);".format(fname, params))
    else:
        print("return mpi_wtime_();")
    print("}")



def is_part_of_bindings(f):
    return f.isbindings() and \
        f.isf90() and \
        not f.isf08conv() and \
        not f.isvariadic()

a.forall(gen_fortran_iface, is_part_of_bindings)

