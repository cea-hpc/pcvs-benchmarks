import json
import random
import re
import copy
from bindingtypes import *

class MPI_Standard_meta():

    def __init__(self, lang="std", fprefix="", fsuffix="", mpi_version="4.0.0"):
        self._varray = [int(c) for c in mpi_version.split('.')]
        
        self._stdkindmap = {
            "standard": {
                'c': BIG_C_KIND_MAP,
                'f': BIG_F90_KIND_MAP,
                'f08': BIG_F08_KIND_MAP,
            },
            "legacy": {
                'c': SMALL_C_KIND_MAP,
                'f': SMALL_F90_KIND_MAP,
                'f08': SMALL_F08_KIND_MAP
            }
        }
        
        self.lang = lang
        self.fprefix = fprefix
        self.fsuffix = fsuffix

    def fname(self, name):
        return self.fprefix + name + self.fsuffix

    def _get_proper_kindmap(self, version):
        version = [int(c) for c in version.split('.')]
        res = [(version[i]-self._varray[i])*(1000/(10**i)) for i in range(0, len(version))]
        return self._stdkindmap['standard'] if sum(res) >= 0 else self._stdkindmap['legacy']

    def _kind_expand_c(self, kind, version="4.0.0"):
        map = self._get_proper_kindmap(version)['c']
        if kind in map:
            return map[kind]
        else:
            return "ERR"
        
    def _kind_expand_f(self, kind, ver='f', version="4.0.0"):
        mapkind = self._get_proper_kindmap(version)
        map = mapkind['f08'] if ver == 'f08' else mapkind['f']
                 
        if kind in map:
            value = map[kind]
            if value:
                repl = {
                    'TYPE(*)': "TYPE(INTEGER)",
                    'DIMENSION(..)': "DIMENSION(10)",
                    '<type>': "TYPE(INTEGER)",
                    'CHARACTER*(*)': 'CHARACTER*(10)'
                }
                for k, v in repl.items():
                    value = value.replace(k, v)

                return value
            else:
                return ""
        else:
            return "ERR"

    def kind_expand(self, kind, lang=None, version="4.0.0"):
        if lang is None:
            lang = self.lang
        if lang == "std":
            return kind
        elif lang == "c" or lang == "fbind":
            return self._kind_expand_c(kind, version=version)
        elif lang in ["f", "f77", "f90", "f08"]:
            return self._kind_expand_f(kind, lang, version=version)
        else:
            print(lang)
            raise Exception("No such kind expand")


class MPI_Parameter():

    def __init__(self, content, meta=None):
        self.meta = meta
        self.content = content.copy()
        self.fbind_type = None
        self.fbind_noderef = False
        self.fbind_getref=False

    def _get_attr(self, attr):
        if attr in self.content:
            return self.content[attr]
        else:
            return None

    def set_extern_fbind_type(self, type):
        self.fbind_type = type

    def ishandle(self):
        handle_kind = ["INFO",
                       "STATUS",
                       "REQUEST",
                       "OPERATION",
                       "DATATYPE",
                       "GROUP",
                       "WINDOW",
                       "FILE",
                       "ERRHANDLER",
                       "COMMUNICATOR"]
        return self.kind() in handle_kind

    def ispoly(self):
        return "POLY" in self.kind() and self.kind() != "POLYFUNCTION"

    def pointer(self):
        return self._get_attr("pointer")

    def length(self):
        return self._get_attr("length")

    def array_length(self):
        return isinstance(self._get_attr("length"), list)

    def attr(self, k):
        return self._get_attr(k)
    
    def _get_c_pointer(self):

        if self.meta.lang != "c" and self.meta.lang != "fbind":
            return ''

        if self.pointer() is not None and not self.pointer():
            return ''

        if self.kind() == 'STRING_2DARRAY':
            return '**'

        if self.kind() == 'ARGUMENT_LIST':
            return '***'

        # needed for MPI_UNPACK_EXTERNAL[_size]
        if (self.kind() == 'STRING' and
            self.length() == '*' and
                not self.pointer()):
            return ''

        if self.kind() in ('BUFFER', 'C_BUFFER', 'C_BUFFER2', 'C_BUFFER3',
                                    'C_BUFFER4', 'EXTRA_STATE',
                                    'EXTRA_STATE2', 'ATTRIBUTE_VAL', 'STATUS',
                                    'ATTRIBUTE_VAL_10', 'STRING', 'STRING_ARRAY',
                                    'FUNCTION', 'FUNCTION_SMALL', 'POLYFUNCTION',
                                    'TOOL_MPI_OBJ', 'F08_STATUS', 'F90_STATUS'):
            return '*'

        if (self.content['param_direction'] == 'inout' or
                self.content['param_direction'] == 'out' or
                self.content['pointer']) and self.content['length'] is None:
            return '*'

        return ''

    def get_c_array(self, dflt_size=""):
        if self.meta.lang != "c" and self.meta.lang != "fbind":
            return ''
        # Add "[]" if:
        # - This is not a STRING, and
        # - length > 0, and
        # - "pointer" was not specified
        # OR
        # - The type is STRING_ARRAY or STRING_2DARRAY

        if self.kind() == 'C_BUFFER4':
            # length set on MPI_User_function
            return ''

        if ( self.kind() != 'STRING' and
                self.length() is not None and
                not isinstance(self.length(), list) and
                not self.pointer()):
            return '[{}]'.format(dflt_size)

        # required by MPI_UNPACK_EXTERNAL, it uses array notation for a string.
        if ( self.kind()  == 'STRING' and
                self.length() == '*' and
                not self.pointer()):
            return '[{}]'.format(dflt_size)

        if ( self.kind()  == 'STRING_ARRAY' or
                 self.kind()  == 'STRING_2DARRAY'):
            return '[{}]'.format(dflt_size)

        # As of MPI-4.0, we have array parameters with -- at most -- 2
        # dimensions.  Always print the first dimension as [] (above).
        # If we have a second dimension, print it (e.g.,
        # MPI_GROUP_RANGE_INCL & EXCL).
        if isinstance(self.length(), list):
            return '[{dflt}][{len}]'.format(dflt=dflt_size, len=self.length()[1])

        return ''


    def gen_f_funcdef(self, varname, lang="f"):
        
        user_func_decls = {
                "MPI_Comm_copy_attr_function": [
                """(oldcomm, comm_keyval, extra_state, attribute_val_in, &
                    & attribute_val_out, flag, ierror)
                    import MPI_Comm, MPI_ADDRESS_KIND
					TYPE(MPI_Comm) :: oldcomm
					INTEGER :: comm_keyval, ierror
					INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
					LOGICAL :: flag""",
                """(OLDCOMM, COMM_KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN, &
                    & ATTRIBUTE_VAL_OUT, FLAG, IERROR)
                    INTEGER OLDCOMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE, ATTRIBUTE_VAL_IN,
                    ATTRIBUTE_VAL_OUT
                    LOGICAL FLAG"""
                ],
                "MPI_Comm_delete_attr_function": [
                """(comm, comm_keyval, attribute_val, extra_state, ierror)
                    import MPI_Comm, MPI_ADDRESS_KIND
                    TYPE(MPI_Comm) :: comm
                    INTEGER :: comm_keyval, ierror
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state""",
                """(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERROR)
                    INTEGER COMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL, EXTRA_STATE"""
                ],
                "MPI_Win_copy_attr_function": [
                """(oldwin, win_keyval, extra_state, attribute_val_in, &
                    & attribute_val_out, flag, ierror)
                    import MPI_Win, MPI_ADDRESS_KIND
					TYPE(MPI_Win) :: oldwin
					INTEGER :: win_keyval, ierror
					INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
					LOGICAL :: flag""",
                """(OLDCOMM, COMM_KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN, &
                    & ATTRIBUTE_VAL_OUT, FLAG, IERROR)
                    INTEGER OLDCOMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE, ATTRIBUTE_VAL_IN,
                    ATTRIBUTE_VAL_OUT
                    LOGICAL FLAG"""
                ],
                "MPI_Win_delete_attr_function": [
                """(win, win_keyval, attribute_val, extra_state, ierror)
                    import MPI_Win, MPI_ADDRESS_KIND
                    TYPE(MPI_Win) :: win
                    INTEGER :: win_keyval, ierror
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state""",
                """(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERROR)
                    INTEGER COMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL, EXTRA_STATE"""
                ],
                "MPI_Type_copy_attr_function": [
                """(oldtype, type_keyval, extra_state, attribute_val_in, &
                    & attribute_val_out, flag, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                     TYPE(MPI_Datatype) :: oldtype
                     INTEGER :: type_keyval, ierror
                     INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
                     LOGICAL :: flag""",
                """(OLDCOMM, COMM_KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN, &
                    & ATTRIBUTE_VAL_OUT, FLAG, IERROR)
                    INTEGER OLDCOMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE, ATTRIBUTE_VAL_IN,
                    ATTRIBUTE_VAL_OUT
                    LOGICAL FLAG"""
                ],
                "MPI_Type_delete_attr_function": [
                """(type, type_keyval, attribute_val, extra_state, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                    TYPE(MPI_Datatype) :: type
                    INTEGER :: type_keyval, ierror
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state""",
                """(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERROR)
                    INTEGER COMM, COMM_KEYVAL, IERROR
                    INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL, EXTRA_STATE"""
                ],
                "MPI_Copy_function": [
                """(OLDCOMM, KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN, &
                    & ATTRIBUTE_VAL_OUT, FLAG, IERR)
                    INTEGER OLDCOMM, KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN,
                    ATTRIBUTE_VAL_OUT, IERR
                    LOGICAL FLAG""",
                ],
                "MPI_Delete_function": [
                """(COMM, KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERR)
                    INTEGER COMM, KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERR"""
                ],
                "MPI_User_function": [
                """(invec, inoutvec, len, datatype)
                    import MPI_Datatype, C_PTR
                    TYPE(C_PTR), VALUE :: invec, inoutvec
                    INTEGER :: len
                    TYPE(MPI_Datatype) :: datatype""",
                """(INVEC, INOUTVEC, LEN, DATATYPE)
                INTEGER INVEC(LEN), INOUTVEC(LEN)
                INTEGER LEN, DATATYPE"""
                ],
                "MPI_Datarep_extent_function": [
                """(datatype, extent, extra_state, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                    TYPE(MPI_Datatype) :: datatype
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
                    INTEGER :: ierror""",
                """(DATATYPE, EXTENT, EXTRA_STATE, IERROR)
                INTEGER DATATYPE, IERROR
                INTEGER(KIND=MPI_ADDRESS_KIND) EXTENT, EXTRA_STATE"""
                ],
                "MPI_Datarep_conversion_function": [
                """(userbuf, datatype, count, &
                & filebuf, position, extra_state, ierror)
                import MPI_Datatype, MPI_OFFSET_KIND, MPI_ADDRESS_KIND, C_PTR
                TYPE(C_PTR), VALUE :: userbuf, filebuf
                TYPE(MPI_Datatype) :: datatype
                INTEGER :: count, ierror
                INTEGER(KIND=MPI_OFFSET_KIND) :: position
                INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state""",
                """(USERBUF, DATATYPE, COUNT, FILEBUF, POSITION, &
                & EXTRA_STATE, IERROR)
                INTEGER USERBUF(10), FILEBUF(10)
                INTEGER DATATYPE, COUNT, IERROR
                INTEGER(KIND=MPI_OFFSET_KIND) POSITION
                INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE"""
                ],
                "MPI_Comm_errhandler_function": [
                """(comm, error_code)
                    import MPI_Comm
                    TYPE(MPI_Comm) :: comm
                    INTEGER :: error_code""",
                """(COMM, ERROR_CODE)
                    INTEGER COMM, ERROR_CODE"""
                ],
                "MPI_Session_errhandler_function": [
                """(comm, error_code)
                    import MPI_Session
                    TYPE(MPI_Session) :: comm
                    INTEGER :: error_code""",
                """(COMM, ERROR_CODE)
                    INTEGER COMM, ERROR_CODE"""
                ],
                "MPI_Win_errhandler_function": [
                """(comm, error_code)
                    import MPI_Win
                    TYPE(MPI_Win) :: comm
                    INTEGER :: error_code""",
                """(COMM, ERROR_CODE)
                    INTEGER COMM, ERROR_CODE"""
                ],
                "MPI_File_errhandler_function": [
                """(comm, error_code)
                    import MPI_File
                    TYPE(MPI_File) :: comm
                    INTEGER :: error_code""",
                """(COMM, ERROR_CODE)
                    INTEGER COMM, ERROR_CODE"""
                ],
                "MPI_Grequest_free_function": [
                """(extra_state, ierror)
                    IMPORT MPI_ADDRESS_KIND
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    INTEGER :: ierror""",
                    """(EXTRA_STATE, IERROR)
                    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
                    INTEGER IERROR"""
                        ],
                "MPI_Grequest_query_function": [
                 """(extra_state, status, ierror)
                    import MPI_ADDRESS_KIND, MPI_Status
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    TYPE(MPI_Status) :: status
                    INTEGER :: ierror""",
                    """(EXTRA_STATE, STATUS, IERROR)
                    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
                    INTEGER STATUS(MPI_STATUS_SIZE), IERROR"""
                        ],
                "MPI_Grequest_cancel_function": [
                """(extra_state, complete, ierror)
                    IMPORT MPI_ADDRESS_KIND
                   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    LOGICAL :: complete
                    INTEGER :: ierror""",
                """(EXTRA_STATE, COMPLETE, IERROR)
                INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
                LOGICAL COMPLETE
                INTEGER IERROR"""
                ]
        }
        target = self._get_attr('func_type')
        assert(target in user_func_decls)
        funcdef = ""
        decls = ""
        if lang == "f08":
            sub_name = "{}_def{}".format(target, random.randint(0, 100))
            funcdef = """
            INTERFACE
            SUBROUTINE {}{}
            END SUBROUTINE
            END INTERFACE""".format(sub_name, user_func_decls.get(target, ("fail", "fail"))[0])
            decls = "PROCEDURE({}), POINTER :: {}".format(sub_name, varname)
        else:
            funcdef = """
subroutine {}(val, ierr)
    integer val, ierr
end subroutine""".format(varname)
            decls = "EXTERNAL {}".format(varname)

        return (funcdef, decls)

    def fbindpointer(self):
        return "*" if (not self._get_c_pointer() and not self.get_c_array()) else ""

    def setfbindnoderef(self):
         self.fbind_noderef=True

    def setfbindgetref(self):
         self.fbind_getref=True

    def desc(self):
        return self._get_attr("desc")

    def intent(self):
        return self._get_attr("param_direction")

    def is_array(self):
        return self.array_dims() is not None

    def array_dims(self):
        l = self._get_attr("length")

        #length may be none but param still an array
        # -> convert
        if not l:
            l = list()

        # special case #1
        if self.kind() == "F90_STATUS":
            l.append("10")
        # special case #2
        elif self.kind() == "ERROR_CODE" and "array" in self.name():
            l.append("10")

        if len(l) <= 0:
            return None

        if not isinstance(l, list):
            l = [l]
        return l

    def isout(self):
        intent = self.intent()
        if intent:
            return intent.endswith("out")

    def isin(self):
        intent = self.intent()
        if intent:
            return intent.startswith("in")

    def constant(self):
        return self._get_attr("constant")

    def kind_expand(self, lang=None, legacy=False):
        if (self.kind() == "FUNCTION") or (self.kind() == "FUNCTION_SMALL") or self.kind() == "POLYFUNCTION":
            return self._get_attr("func_type")
        else:
            return self.meta.kind_expand(self.kind(), lang=lang, version="3.1.0" if legacy else "4.0.0")

    def type_c(self, noconst=False, legacy=False):
        return ("const " if self.constant() and not noconst else "") + self.kind_expand(legacy=legacy)


    def get_f_pointer(self, ver="f"):
        if self.kind() in ['BUFFER', 'C_BUFFER2', 'C_BUFFER3',
                                    'C_BUFFER4',
                                    'STATUS', "STRING_ARRAY"
                                    'ATTRIBUTE_VAL_10']:
            if ver in ['f', 'f77', 'f90']:
                return 10
        return None
    
    def type_decl_f(self, altername=None, lang="f", legacy=False):
        name = altername if altername else self.name()
        
        dims = []
        ptr = self.get_f_pointer(ver=lang)
        if ptr:
            dims.append(str(ptr))
        if self.is_array():
            for d in self.array_dims():
                if d.startswith("MPI_"):
                    dims.append(d)
                elif d.isdigit():
                    dims.append(str(d))
                else:
                    dims.append("10")
        dims = ",".join([elt for elt in dims])
        if lang in ['f08']:
            # special case, declare character array
            if self.kind() == "STRING":
                dims = "({})".format(dims if dims else "10")
            elif self.kind() == "STRING_ARRAY":
                dims = "({}), DIMENSION({})".format(dims if dims else "10", "10")
            elif self.kind() == "STRING_2DARRAY":
                dims = "({}), DIMENSION({}, {})".format(dims if dims else "10", "10", "10")
            elif dims: # otherwise, declare dimensions
                dims = ", DIMENSION({})".format(dims)
            else:
                dims = ""
            return "{}{} :: {}".format(self.kind_expand(lang=lang, legacy=legacy),
                                    dims,
                                    name)
        else:
            if self.kind() in ['STRING', 'STRING_ARRAY', "STRING_2DARRAY"] or not dims:
                # replaced by kind_expand when unrolling the bindings
                dims = ""
            elif dims:
                dims = "(" + dims + ")"

            return "{} {}{}".format(self.kind_expand(lang=lang, legacy=legacy), name, dims)
            

    def type_full_c(self):
        return "{}{}{}".format( self.type_c(),
                                 self._get_c_pointer(),
                                 self.get_c_array())

    def type_c_is_pointer(self):
        return (self._get_c_pointer() == "*")

    def type_decl_c(self, altername=None, default_cnt="2", legacy=False):
        name = altername if altername else self.name()
        return "{} {}{}{}".format( self.type_c(legacy=legacy),
                                   self._get_c_pointer(),
                                   name,
                                   self.get_c_array(default_cnt))

    def str_c(self, legacy=False):
        return "{} {}{}{}".format( self.type_c(legacy=legacy),
                                   self._get_c_pointer(),
                                   self.name(),
                                   self.get_c_array())

    def str_fbind(self):
        
        if self.fbind_type:
            # Was type temporarilly overridden ?
            ftype = self.fbind_type
            self.fbind_type = None
        else:
            ftype = self.kind_expand()

        ftype += self.fbindpointer()
        return "{}{} {}{}{}".format("const " if self.constant() else "",
                                    ftype,
                                    self._get_c_pointer(),
                                    self.name(),
                                    self.get_c_array())

    def __str__(self):
        if self.meta.lang == "c":
            return self.str_c()
        elif self.meta.lang == "fbind":
            return self.str_fbind()
        else:
            return "{} {}".format(self.kind(), self.name())

    def doxygen(self):
        return  " * @param {0} {1}".format(self.name(), self.desc()) 

    def kind(self):
        return self._get_attr("kind")

    def name(self):
        if self.kind() == "VARARGS":
            return ""
        elif self.meta.lang == "fbind" and self._get_attr("name_f90"):
            return self._get_attr("name_f90")
        else:
            return self._get_attr("name")

    def set_name(self, name):
        self.content["name"] = name
    
    def isc(self):
        return not ("c_parameter" in self._get_attr("suppress"))
        
    
    def isf(self):
        return not ("f90_parameter" in self._get_attr("suppress"))
        
    def isf90(self):
        return self.isf()

    def isf08(self):
        return not ("f08_parameter" in self._get_attr('suppress'))


class MPI_Function():

    def _register_parameters(self, meta=None):
        self.parameters = []
        if "parameters" in self.content:
            for p in self.content["parameters"]:
                self.parameters.append(MPI_Parameter(p, meta))

    def __init__(self, content, meta=None):
        self.meta = meta
        self.content = content.copy()
        self._register_parameters(meta)

    def params(self, lang='c'):
        lang = self.meta.lang if not lang else lang
        return [x for x in self.parameters if "{}_parameter".format(lang) not in x._get_attr('suppress')]
        
    def _gen_fbind_paramlist(self):
        # We need to add string suffixes and inline args
        # depending on the fortran support type
        ret = []
        suffix_ret = []
        
        for p in self.parameters:
            if p.kind() == "STRING":
                ret.append("{} CHAR_MIXED(size_{})".format(str(p), p.name()))
                suffix_ret.append("CHAR_END(size_{})".format(p.name()))
            else:
                ret.append(str(p))

        if suffix_ret:
            ret[-1] += " " + " ".join(suffix_ret)

        return ret

    def proto(self, prefix="", suffix="", lowername=False):
        if self.meta.lang == "c":
            str_params = [str(x) for x in self.params()]
        else:
            if self.meta.lang == "fbind":
                str_params = self._gen_fbind_paramlist()
            else:
                str_params = [str(x) for x in self.parameters]

        fname = self.name()
        if lowername:
            fname = fname.lower()

        return "{} {}({})".format(self.meta.kind_expand(self.return_kind()),
                                  prefix + self.meta.fname(fname) + suffix,
                                  ", ".join(str_params))

    def __str__(self):
        return self.proto()

    def _get_attr(self, attr):
        if attr in self.content:
            return self.content[attr]
        else:
            return None

    def attr(self, attr):
        return self._get_attr(attr)

    def isf08conv(self):
        return (self.name().find("f2f08") != -1) or (self.name().find("f082f") != -1)

    def isvariadic(self):
        return len([1 for x in self.parameters if x.kind()=="VARARGS"])


    def _has_ierror(self):
        return len([1 for x in self.parameters if x.name()=="ierror"])

    def return_type(self):
	    return self.meta.kind_expand(self.return_kind())

    def _gen_call_generic(self,
                          param_cb,
                          var = "ret",
                          fprefix="",
                          fsuffix="",
                          rename=None,
                          use_ierror=False,
                          lowername=False):
        if self.return_kind() != "NOTHING":
            has_ret_val = 1

        call=""

        if has_ret_val:
            if use_ierror and self.return_kind() == "ERROR_CODE" and self._has_ierror():
                call += "*ierror = "
            else:
                call+="{} {} = ".format(self.meta.kind_expand(self.return_kind()),
                                    var)

        if rename:
            # Make sure to copy as we edit the array
            loc_params = copy.deepcopy(self.params())
            for p in loc_params:
                name = p.name()
                if name in rename:
                    p.set_name(rename[name])
        else:
            loc_params = self.params()

        str_params = [param_cb(x) for x in loc_params]

        fname = self.meta.fname(self.name())
        if lowername:
            fname = fname.lower()

        call += "{}({});".format(fprefix + fname + fsuffix,
                                 ", ".join(str_params))

        return call

    def _gen_call_c(self, var = "ret", fprefix="", fsuffix="", rename=None, lowername=False):
        def c_param(param):
            return param.name()
        return self._gen_call_generic(c_param, var, fprefix, fsuffix, rename)


    def _gen_call_fbind(self, var = "ret", fprefix="", fsuffix="", rename=None):
        def f_param(param):
                    # We need to add reference to non pointer types
        # and output pointers
            if param.fbind_getref == True:
                ptr = "&"
                param.fbind_getref = False
            elif param.fbind_noderef == True:
                ptr = ""
                param.fbind_noderef = False
            else:
                ptr = param.fbindpointer()
            return ptr + param.name()
        return self._gen_call_generic(f_param, var, fprefix, fsuffix, rename, use_ierror=True)

    def gen_call(self, var="ret", fprefix="", fsuffix="", rename=None):
        if self.meta.lang == "c":
            return self._gen_call_c(var, fprefix, fsuffix, rename)
        elif self.meta.lang == "fbind":
            return self._gen_call_fbind(var, fprefix, fsuffix, rename)
        return ""

    def _gen_return_c(self):
        if self.return_kind() == "NOTHING":
            return ""
        return "return ret;"

    def gen_return(self):
        if self.meta.lang == "c":
            return self._gen_return_c()
        return ""

    def return_kind(self):
        return self._get_attr("return_kind")

    def name(self):
        return self._get_attr("name")

    def iscallback(self):
        attrs = self._get_attr("attributes")
        return attrs["callback"]

    def isbindings(self):
        attrs = self._get_attr("attributes")
        return attrs["lis_expressible"]

    def iswrapper(self):
        return bool(re.fullmatch("^MPI_[a-zA-Z0-9_]+_(c|f(08)*)2(c|f(08)*)$", self.name()))

    def isf(self):
        attrs = self._get_attr("attributes")
        return attrs["f90_expressible"] and not attrs['not_with_mpif']


    def isf90(self):
        attrs = self._get_attr("attributes")
        return attrs["f90_expressible"]

    def isf08(self):
        attrs = self._get_attr("attributes")
        return attrs["f08_expressible"]

    def iscallback(self):
        attrs = self._get_attr("attributes")
        return attrs["callback"]


    def isc(self):
        attrs = self._get_attr("attributes")
        return attrs["c_expressible"]

    def isfile(self):
        return self.name().startswith("MPI_File")

    def isdeprecated(self):
        attrs = self._get_attr("attributes")
        return attrs["deprecated"]


    def number_of_buffer_params(self):
        bcnt = [1 for x in self.parameters if x.kind() == "BUFFER"]
        return sum(bcnt)

    def get_param_by_kind(self, kind):
        return [ x for x in self.parameters if x.kind() == kind]

    def iscollective(self):
        coll_names = ["gather", "scatter", "reduce", "bcast", "alltoall", "barrier"]
        lname = self.name().lower()
        for cn in coll_names:
            if cn in lname:
                return 1
        return 0

    def isinit(self):
        return self.name() in ["MPI_Init", "MPI_Init_thread"]

    def doxygen(self):
        brief = "MPI function {}".format(self.name())
        params = "\n".join([x.doxygen() for x in self.params()])
        if not params:
            params = " *"
        returnt = " *"
        returndesc = "\n"
        if self.return_kind() != "NOTHING":
            returnt = " * @return " + self.meta.kind_expand(self.return_kind())
        
        if self.return_kind() == "ERROR_CODE":
            returndesc="MPI_SUCCESS on success other MPI_* error code otherwise"

        return """
/**
 * @brief {0}
 * 
{1}
 *
{2} {3} 
 */""".format(brief, params, returnt, returndesc)

    def forall_params(self, callback, filter_callback=None):
        for p in self.parameters:
            if filter_callback and not filter_callback(p):
                continue
            callback(p)

# Cleang regexpr \\\\[a-z]+\{([^\}]+)\} $1

class MPI_Interface():

    def _load_content(self, f):
            if isinstance(f, str):
                f = open(f, 'r')
            self.standard_content = json.load(f)

    def _load_function(self, meta=None):
        self.functions = {}
        for k, v in self.standard_content.items():
            self.functions[k] = MPI_Function(v, meta)

    def __init__(self, path, meta=MPI_Standard_meta()):
        self.meta = meta
        self._load_content(path)
        self._load_function(meta)

    def get(self, funcname):
        return self.functions[funcname]

    def forall(self, callback, filter_callback=None):

        # Do some sorting for more elegant output
        func = [ f for f in self.functions.values()]
        func.sort(key=lambda f: f.name())

        for f in func:
            if filter_callback and not filter_callback(f):
                continue
            callback(f)

