import os
import textwrap
import json
import re
import copy
import random
from ruamel.yaml import YAML
from mpiiface import MPI_Interface, MPI_Standard_meta, MPI_Function, MPI_Parameter

SRCDIR="."
OUTDIR="./tests/"
standards = {}
yaml_handlers = {}

def compute_min_std_from_tag(tags):
    stds = []
    for x in tags:
        if x.startswith("STD:"):
            stds.append(float(x.split(":")[1]))
    if len(stds) == 0:
        return 'WARN:UNTAGGED'
    return min(stds)

def get_func_tags(s, std_hint=None):
    global funcs
    try:
        return funcs[s]
    except KeyError:
        return []

def isascii(s):
    return len(s) == len(s.encode())

def valid(s):
    return ascii(s) and s.lower() not in ["int", "double", "integer", "real", "long"]

def param_counter_by_kind(func, pattern=None):
    return len([param for param in func.params() if not pattern or pattern in param.kind()])

def build_c_code(func, decls, calls):
    code_decls = []
    code_calls = []
    has_ret = False
    params = []
    new_decls = []
    for d in decls:
        varname = d[0]
        param = d[1]
        legacymode = d[2] if len(d) >= 3 else False
        if not param.isc():
            continue
        if param.kind() == "VARARGS":
            code_decls.append("char* {}[10];".format(varname))
        else:
            code_decls.append("{};".format(param.type_decl_c(varname, legacy=legacymode)))
        params.append(varname)
    
    if func.return_kind() != "NOTHING":
        code_decls.append("{} ret;".format(func.meta.kind_expand(func.return_kind(), lang="c")))
        has_ret = True
    
    for c in calls:
        if has_ret:
            code_calls.append("ret = {}({});".format(c, ", ".join(params)))
        else:
            code_calls.append("(void) {}({});".format(c, ", ".join(params)))
    return """#include <mpi.h>
int main(char argc, char**argv)
{{
    /* vars */
    {}
    /* calls */
    {}
    return 0;
}}
""".format("\n    ".join(code_decls), "\n    ".join(code_calls))


def _build_f_code(func, decls, calls, include=None, lang=None):
    params = []
    has_ret = False
    if not include:
        include = "include 'mpif.h'"

    if not lang:
        lang = "f"
        
    code_decls = []
    code_calls = []
    prepend = []
    postpend = []
    for d in decls:
        varname = d[0]
        param = d[1]
        legacymode = d[2] if len(d) >= 3 else False
        if param.kind() == "VARARGS":
            continue
        # hack to manager user function for some fortran examples
        if "FUNCTION" in param.kind():
            datatype = None
            for v in ["MPI_Comm", "MPI_Win", "MPI_Session", "MPI_File"]:
                if func.name().startswith(v):
                        datatype = v
            (fcdef, extra_decl) = param.gen_f_funcdef(varname, lang=lang)
            if lang == "f08":
                postpend.append(fcdef)
            else:
                prepend.append(fcdef)
        else:
            extra_decl = "{}".format(param.type_decl_f(varname, lang=lang, legacy=legacymode))
        
        code_decls.append(extra_decl)
        params.append(varname)
    
    if func.return_kind() not in ["NOTHING", "ERROR_CODE"]:
        code_decls.append("{} ret".format(func.meta.kind_expand(func.return_kind(), lang=lang)))
        has_ret = True

    for c in calls:
        if has_ret:
            line = "ret = {}({})".format(c.lower(), ", ".join(params))
        else:
            line = "call {}({})".format(c.lower(), ", ".join(params))
        chunks = textwrap.wrap(line, width=90)
        for idx, chunk in enumerate(chunks):
            if idx == len(chunks) -1:
                code_calls.append(chunk)
            else:
                code_calls.append(chunk + " &")

    return """
        {prepend}
        program main
        {inc}
        {postpend}
        {decl}
        {call}
        end program main
    """.format(
            prepend="\n".join(prepend),
            postpend="\n".join(postpend),
            inc=include,
            decl="\n       ".join(code_decls),
            call="\n       ".join(code_calls))

def build_f77_code(func, decls, calls):
    new_decls = []
    for idx, value in enumerate(decls):
        name, param, _ = value
        if param.isf():
            new_decls.append(value)
    return _build_f_code(func, new_decls, calls, lang="f90", include= "include 'mpif.h'")

def build_f90_code(func, decls, calls):
    new_decls = []
    for idx, value in enumerate(decls):
        name, param, _ = value
        if param.isf90():
            new_decls.append(value)
    return _build_f_code(func, new_decls, calls, lang="f90", include="use mpi")

def build_f08_code(func, decls, calls):
    new_decls = []
    for idx, value in enumerate(decls):
        name, param, _ = value
        if param.isf08():
            new_decls.append(value)
    return _build_f_code(func, new_decls, calls, lang="f08", include="use mpi_f08")

def dump_code(func, filepath, decls, calls, lang="c"):
    content = ""
    if lang == "c":
        content = build_c_code(func, decls, calls)
    elif lang == "f77":
        content = build_f77_code(func, decls, calls)
    elif lang == "f90":
        content = build_f90_code(func, decls, calls)
    elif lang == 'f08':
        content = build_f08_code(func, decls, calls)
    
    assert (content)
    create_path_to_file(filepath)

    with open(filepath, "w") as fh:
        fh.write(content)

def create_path_to_file(f):
    path = os.path.dirname(f)
    if not os.path.isdir(path):
        os.makedirs(path)
    else:
        # should remove before ?
        pass


def dump_yaml(nodename, rev, srcfile, tags, lang="c"):
        global OUTDIR
        # then, append to associated YAML
        extra_flags = "-ffree-form" if lang in ['f', 'f77', 'f90'] else ""
        target_yaml = "functions/{}".format(rev)
        
        if target_yaml not in yaml_handlers.keys():
            target_path = os.path.join(OUTDIR, "functions", str(rev),  "pcvs.yml")
            create_path_to_file(target_path)
            hdl = open(target_path, "w")
            yaml_handlers[target_yaml] = hdl
        else:
            hdl = yaml_handlers[target_yaml]


        YAML().dump({"{}".format(nodename): {
                "tag": tags,
                "build": {
                    "files": ["{}".format(srcfile)],
                    "sources": {
                        "cflags": "-Wno-deprecated-declarations -Werror -Wno-error=line-truncation {}".format(extra_flags)
                    }
                }
            }
        }, hdl)

def process_function(func):
    global OUTDIR, yaml_handlers
    if not func.isc():
        return
    # first, create source file
    decls = []
    decls_large = []
    calls = []
    calls_large = []
    picked_names = []
    # 4 scenarios (2x2):
    #  - Function has a 'largecount' version -> any 'POLY' param type
    #  - A param is exclusive to the 'largecount' version (large_only is
    #    True)
    # this leads to build the following:
    # (decls,parameters) contains attributes for regular calls
    # (decls_large,parameters_large) contains attributes for largecount
    # calls
    # To generate the 4 scenarios:
    # if func() has a largecount versions:
    #    - decls & func(parameters) (BUT DROP 'large_only' params !)
    #    - decls_large & func_c(parameter_large)
    # if func() doesn't have a largecount version:
    #    - decls & func(parameters)
    #    - *_large lists SHOULDN'T be set as no param should have the 'POLY'
    #      type
    largecount_alternate_func = param_counter_by_kind(func, "POLY") > 0
    i = 0
    for param in func.params(lang='std'):
        varname = "{}_{}".format("var", str(i))
        i += 1
        picked_names.append(varname)
        
        #build the only case where a '_c' function is built with
        # a different set of parameters
        # -> take ALL args (there is no attribute to set parameters that
        # SHOULD NOT be part of largecount prototypes)
        if largecount_alternate_func:
            decls_large.append((varname, param, False))
       
        #  Now, attenmpt to build two scenarios at once:
        # either (largecount func & not 'large_only' parameter) or not
        # largecount func -> pick up
        if largecount_alternate_func and not param.attr('large_only') or not largecount_alternate_func:
            decls.append((varname, param, True))

    calls.append("{}".format(func.name()))
    calls.append("P{}".format(func.name()))

    if decls_large:
        calls_large.append("{}_c".format(func.name()))
        calls_large.append("P{}_c".format(func.name())) 
            
    
    comb_list = []
    if func.isc():
        comb_list.append(("c", ".c"))
    if func.isf():
        comb_list.append(("f77", ".f"))
    if func.isf90():
        comb_list.append(("f90", ".f90"))
    if func.isf08():
        comb_list.append(("f08", ".f08"))

    for i in comb_list:
        lang = i[0]
        ext = i[1]
        func_tags = get_func_tags(func.name())
        rev_tag = compute_min_std_from_tag(func_tags)
        srcpath = os.path.join(OUTDIR,
            "functions",
            "{}".format(str(rev_tag)),
            "{}".format(func.name())
        )
        dump_code(func, srcpath + ext, decls, calls, lang=lang)
        dump_yaml("{}_lang{}".format(func.name(), lang), rev_tag, os.path.basename(srcpath + ext),
                  lang=lang, tags=[lang, "functions", *func_tags])
    
        if largecount_alternate_func:
            if lang == "f08":
                # f08 large counts are handled through polymorphism
                calls_large = calls
            elif lang in ["f90", "f77"]:
                continue

            large_func_tags = get_func_tags("{}_c".format(func.name()))
            large_rev_tag = compute_min_std_from_tag(large_func_tags)
            large_srcpath = os.path.join(OUTDIR,
                            "functions",
                            "{}".format(str(large_rev_tag)),
                            "{}_c".format(func.name())
            )
            dump_code(func, large_srcpath + ext, decls_large, calls_large, lang=lang)
            dump_yaml("{}_c_lang{}".format(func.name(), lang), large_rev_tag,
                      os.path.basename(large_srcpath + ext),
                      lang=lang, tags=[lang, 'functions', 'large_count', *large_func_tags])


def classify_functions_per_standard():
    
    global SRCDIR, OUTDIR, standards
    
    for d in ["functions"]:
        path = os.path.join(SRCDIR, "classification", d)
        for f in os.listdir(path):
            catch = re.match("functions.(\d).x", f)
            if not catch:
                raise ValueError()
            else:
                std = catch.group(1)    
                target = catch.group(3) if len(catch.groups()) >= 2 else "add"
            
            standards.setdefault(std, {'add': [], 'rm': []})
            
            with open(os.path.join(path, f), 'r') as fh:
                content = fh.read().strip().split("\n")
                for tags in [l.split(" ", 1) for l in content]:
                    standards[std][target].append(tags[0])
                    if len(tags) <= 1: 
                        continue
                    if tags[0] not in func_tags:
                        func_tags[tags[0]] = tags[1:]
                    
            prefix = os.path.join(OUTDIR, d, std)
            os.makedirs(prefix, exist_ok = True)
            #for ext in ['c', 'F', 'f90', 'f08']:
            #    srcfile = os.path.join(os.path.abspath(SRCDIR), d+"."+ext)
            #    dstfile = os.path.join(os.path.abspath(prefix), d+"."+ext)
            #    if not os.path.exists(dstfile):
            #        os.symlink(srcfile, dstfile)
            yaml_handlers["{}/{}".format(d, std)] = open(os.path.join(prefix, "pcvs.yml"), "w")

def is_part_of_bindings(f):
    return (f.isbindings() or f.iswrapper()) and \
        f.isc() and \
		not f.iscallback()

def load_functions(f):
    global funcs
    with open(f, 'r') as fh:
        funcs = json.load(fh)

load_functions(os.path.join(SRCDIR, "utils", "standard_level.json"))

#classify_functions_per_standard()
a = MPI_Interface(os.path.join(SRCDIR, "./prepass.dat"), MPI_Standard_meta(lang="c", mpi_version="4.0.0"))
a.forall(process_function, is_part_of_bindings)
