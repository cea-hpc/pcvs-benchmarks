"""
Defines the binding types for the MPI bindings generation tool.
"""


# flake8: noqa: E302

import copy


LIS_KIND_MAP = {
    # Pointers
    'BUFFER'              : 'choice',
    'C_BUFFER'            : 'choice',

    # C_BUFFER2 is required for MPI_Buffer_detach, F90 is different
    # from C_BUFFER
    'C_BUFFER2'           : 'choice',

    # C_BUFFER3 is required for MPI_CONVERSION_FN_NULL
    'C_BUFFER3'           : None,

    # C_BUFFER4 is required for MPI_User_function
    'C_BUFFER4'           : None,

    'EXTRA_STATE'         : None,
    'EXTRA_STATE2'        : None,

    'FUNCTION_SMALL'      : 'function',
    'FUNCTION'            : 'function',
    'POLYFUNCTION'        : 'function',

    # Callback-functions for MPI_T events
    'EVENT_CB_FUNCTION'      : 'function',
    'EVENT_FREE_CB_FUNCTION' : 'function',
    'EVENT_DROP_CB_FUNCTION' : 'function',

    'STRING'              : 'string',
    'STRING_ARRAY'        : 'array of strings',
                                      # 1D array of strings, as in
                                      # MPI_COMM_SPAWN
    'STRING_2DARRAY'      : 'array of array of strings',
                                      # 2D array of strings, as in
                                      # MPI_COMM_SPAWN_MULTIPLE

    # C ONLY ARGUMENT PARAMETERS
    'ARGUMENT_COUNT'      : None,
    'ARGUMENT_LIST'       : None,

    # Various types of integers
    # NNI is non-negative integer
    'ARRAY_LENGTH'        : 'integer',
    'ARRAY_LENGTH_NNI'    : 'non-negative integer',
    'ARRAY_LENGTH_PI'     : 'positive integer',
    'ATTRIBUTE_VAL_10'    : None, # From MPI-1.0
    'ATTRIBUTE_VAL'       : None, # Current version of MPI
    'BLOCKLENGTH'         : 'non-negative integer',
    'COLOR'               : 'integer',
    'COORDINATE'          : 'integer',
    'COORDINATE_NNI'      : 'non-negative integer',
    'DEGREE'              : 'non-negative integer',
    'DIMENSION'           : 'integer',
    'ENUM'                : 'integer',
    'FILE_DESCRIPTOR'     : 'integer',
    'KEY'                 : 'integer',
    'KEYVAL'              : 'integer',
    'INDEX'               : 'integer',
    'LOGICAL'             : 'logical',
    'LOGICAL_OPTIONAL'    : 'integer',
    'LOGICAL_BOOLEAN'     : 'boolean',
    'MATH'                : 'integer',
    'NUM_DIMS'            : 'integer',
    'RANK'                : 'integer',
    'RANK_NNI'            : 'non-negative integer',
    'COMM_SIZE'           : 'integer',
    'COMM_SIZE_PI'        : 'positive integer',
    'STRING_LENGTH'       : 'integer',
    'STRIDE_BYTES'        : 'integer',
    'STRIDE_ELEM'         : 'integer',
    'TAG'                 : 'integer',
    'VERSION'             : 'integer',
    'WEIGHT'              : 'non-negative integer',
    'OFFSET'              : 'integer',
    'PROFILE_LEVEL'       : 'integer',
    'ASSERT'              : 'integer',
    'WINDOW_SIZE'         : 'non-negative integer',
    'INFO_VALUE_LENGTH'   : 'integer',
    'ACCESS_MODE'         : 'integer',
    'KEY_INDEX'           : 'integer',
    'TOOLENUM_INDEX'      : 'integer',
    'TOOLENUM_SIZE'       : 'integer',
    'TOOL_VAR_VERBOSITY'  : 'integer',
    'TOOL_VAR_VALUE'      : 'integer',
    'CVAR_INDEX'          : 'integer',
    'CVAR_INDEX_SPECIAL'  : 'index',
    'PVAR_INDEX'          : 'integer',
    'PVAR_CLASS'          : 'integer',
    'SOURCE_INDEX'        : 'integer',
    'EVENT_INDEX'         : 'integer',
    'CAT_INDEX'           : 'integer',
    'UPDATE_NUMBER'       : 'integer',
    'DROPPED_COUNT'       : 'positive integer',
    'TYPECLASS_SIZE'      : 'integer',
    'GENERIC_DTYPE_INT'   : 'integer',
    'GENERIC_DTYPE_COUNT' : 'integer',
    'PROCESS_GRID_SIZE'   : 'positive integer',
    'DTYPE_DISTRIBUTION'  : 'positive integer',

    # The MPI-3.1 binding for MPI_ALLOC_MEM uses MPI_Aint for the
    # number of bytes, which is arguably wrong (it really should be
    # MPI_Count).  So we create a type just for the ALLOC_MEM binding,
    # a) to call out its Wrongness, and b) so that nothing else will
    # be tainted by its Wrongness.
    # Improved understanding of the union-like nature of MPI_Aint informs us
    # using MPI_Aint for MPI_ALLOC_MEM is correct and no changes are needed.
    'ALLOC_MEM_NUM_BYTES' : 'non-negative integer',
    # Ditto for PACK_EXTERNAL functions' "size" param: it's MPI_Aint,
    # but really should be MPI_Count.
    'PACK_EXTERNAL_SIZE'  : 'integer',
    # Similarly for MPI_Win_attach, using MPI_Aint is the correct big type.
    'WIN_ATTACH_SIZE'     : 'non-negative integer',

    # From Dan: "Anything that is described a distance from a baseptr
    # or address is likely to be a displacement".

    # MPI-3.1 defined some displacement parameters as "small" integers
    # and some as "big" integers.  This type denotes the parameters
    # that are "big" (i.e., MPI_Aint), and therefore do not need to be
    # polymorphic (to support both old/small and new/big) -- because
    # they are already big.
    'DISPLACEMENT_SMALL'  : 'integer',
    'DISPLACEMENT'        : 'integer',
    'DISPLACEMENT_NNI'    : 'non-negative integer',
    'POLYDISPLACEMENT'    : 'integer',

    # Similarly, there are some MPI-3.1 functions that have a "number
    # of elements" type of parameter that a "big" type, and do not
    # need to be polymorphic.

    'NUM_BYTES_SMALL'     : 'integer',
    'NUM_BYTES'           : 'integer',

    'NUM_BYTES_NNI_SMALL' : 'non-negative integer',
    'NUM_BYTES_NNI'       : 'non-negative integer',

    # Enums
    'ERROR_CODE'            : 'integer',
    'ERROR_CODE_SHOW_INTENT': 'integer',
    'ERROR_CLASS'           : 'integer',
    'ORDER'                 : 'state',
    'THREAD_LEVEL'          : 'integer',
    'COMBINER'              : 'state',
    'LOCK_TYPE'             : 'state',
    'TOOLS_ENUM'            : 'handle',
    'UPDATE_MODE'           : 'state',
    'BIND_TYPE'             : 'integer',
    'SOURCE_ORDERING'       : 'integer',
    'CALLBACK_SAFETY'       : 'integer',
    'VARIABLE_SCOPE'        : 'integer',
    'TYPECLASS'             : 'integer',
    'GROUP_COMPARISON'      : 'integer',
    'COMM_COMPARISON'       : 'integer',
    'SPLIT_TYPE'            : 'integer',
    'TOPOLOGY_TYPE'         : 'state',
    'DISTRIB_ENUM'          : 'state',

    # Polymorphic types and their corresponding non-polymorphic types.
    # Anything that is POLY* means that it has one type in <=MPI-3.1
    # and a different type in >=MPI-4.0.

    'RMA_DISPLACEMENT_SMALL': 'positive integer',
    'RMA_DISPLACEMENT'      : 'positive integer',
    'POLYRMA_DISPLACEMENT'  : 'positive integer',

    'RMA_DISPLACEMENT_NNI_SMALL': 'non-negative integer',
    'RMA_DISPLACEMENT_NNI'      : 'non-negative integer',
    'POLYRMA_DISPLACEMENT_NNI'  : 'non-negative integer',

    # For datatype extent (e.g., MPI_File_get_type_extent)
    'DISPOFFSET_SMALL'    : 'integer',
    'DISPOFFSET'          : 'integer',
    'POLYDISPOFFSET'      : 'integer',

    # For datatype constructors (e.g., MPI_Type_create_subarray)
    'DTYPE_NUM_ELEM_NNI_SMALL': 'non-negative integer',
    'DTYPE_NUM_ELEM_NNI'      : 'non-negative integer',
    'POLYDTYPE_NUM_ELEM_NNI'  : 'non-negative integer',

    'DTYPE_NUM_ELEM_SMALL': 'integer',
    'DTYPE_NUM_ELEM'      : 'integer',
    'POLYDTYPE_NUM_ELEM'  : 'integer',

    'DTYPE_NUM_ELEM_PI_SMALL' : 'positive integer',
    'DTYPE_NUM_ELEM_PI'       : 'positive integer',
    'POLYDTYPE_NUM_ELEM_PI'   : 'positive integer',

    # introduced for the datatypes chapter
    'DTYPE_STRIDE_BYTES_SMALL': 'integer',
    'DTYPE_STRIDE_BYTES'      : 'integer',
    'POLYDTYPE_STRIDE_BYTES'  : 'integer',

    'DISPLACEMENT_COUNT_SMALL': 'integer',
    'DISPLACEMENT_COUNT'      : 'integer',
    'POLYDISPLACEMENT_COUNT'  : 'integer',

    'DISPLACEMENT_AINT_COUNT_SMALL': 'integer',
    'DISPLACEMENT_AINT_COUNT'      : 'integer',
    'POLYDISPLACEMENT_AINT_COUNT'  : 'integer',

    'DTYPE_PACK_SIZE_SMALL': 'integer',
    'DTYPE_PACK_SIZE'      : 'integer',
    'POLYDTYPE_PACK_SIZE'  : 'integer',

    'LOCATION_SMALL': 'integer',
    'LOCATION'      : 'integer',
    'POLYLOCATION'  : 'integer',

    # For tools interface (e.g. MPI_T_cvar_handle)
    'TOOLS_NUM_ELEM_SMALL': 'integer',
    'TOOLS_NUM_ELEM'      : 'integer',
    'POLYTOOLS_NUM_ELEM'  : 'integer',
    'TOOLS_TICK_COUNT'    : 'integer',

    # For MPI_Buffer_attach/detach
    'POLYNUM_BYTES'       : 'integer',
    'POLYNUM_BYTES_NNI'   : 'non-negative integer',

    # For transfer procedures (e.g., MPI_Send, MPI_Broadcast, MPI_Put, etc.)
    'XFER_NUM_ELEM_NNI_SMALL': 'non-negative integer',
    'XFER_NUM_ELEM_NNI'      : 'non-negative integer',
    'POLYXFER_NUM_ELEM_NNI'  : 'non-negative integer',

    'XFER_NUM_ELEM_SMALL': 'integer',
    'XFER_NUM_ELEM'      : 'integer',
    'POLYXFER_NUM_ELEM'  : 'integer',

    # special type to avoid semantic conflict between ARRAY_LENGTH_NNI and datatypes usage
    'NUM_PARAM_VALUES_SMALL': 'non-negative integer',
    'POLYNUM_PARAM_VALUES': 'non-negative integer',
    'NUM_PARAM_VALUES': 'non-negative integer',

    # MPI partitioned communication
    'PARTITION'           : 'non-negative integer',

    # MPI handles
    'COMMUNICATOR'        : 'handle',
    'DATATYPE'            : 'handle',
    'ERRHANDLER'          : 'handle',
    'FILE'                : 'handle',
    'GROUP'               : 'handle',
    'INFO'                : 'handle',
    'MESSAGE'             : 'handle',
    'REQUEST'             : 'handle',
    'SESSION'             : 'handle',
    'STATUS'              : 'status',
    'WINDOW'              : 'handle',
    'OPERATION'           : 'handle',
    'CVAR'                : 'handle',
    'PVAR'                : 'handle',
    'PVAR_SESSION'        : 'handle',
    'EVENT_REGISTRATION'  : 'handle',
    'EVENT_INSTANCE'      : 'handle',
    'TOOL_MPI_OBJ'        : 'pointer',

    # Special handles (needed for handle conversion bindings)
    'F90_STATUS'          : "status",
    'F08_STATUS'          : "status",

    'F90_COMM'            : None,
    'F90_DATATYPE'        : None,
    'F90_GROUP'           : None,
    'F90_REQUEST'         : None,
    'F90_FILE'            : None,
    'F90_WIN'             : None,
    'F90_OP'              : None,
    'F90_INFO'            : None,
    'F90_ERRHANDLER'      : None,
    'F90_MESSAGE'         : None,
    'F90_SESSION'         : None,

    # Special handle for VARARGS in MPI_Pcontrol
    'VARARGS'             : '...',

    # Specials for return types
    'WALL_TIME'           : None,
    'TICK_RESOLUTION'     : None,
    'NOTHING'             : None
}

#------------------------------------

# This map is not to be used directly -- it is copied and used as the
# basis for multiple other kind maps that fill in proper types for the
# POLY* types.

BASE_C_KIND_MAP = {
    # Pointers
    'BUFFER'              : 'void',
    'C_BUFFER'            : 'void',
    'C_BUFFER2'           : 'void',
    'C_BUFFER3'           : 'void',
    'C_BUFFER4'           : 'void',
    'EXTRA_STATE'         : 'void', # The '*' is added in bindingc.py
    'EXTRA_STATE2'        : 'void',
    'FUNCTION_SMALL'      : None,   # Every function pointer type is different
    'FUNCTION'            : None,
    'POLYFUNCTION'        : None,

    # Callback-functions for MPI_T events
    'EVENT_CB_FUNCTION'      : 'MPI_T_event_cb_function',
    'EVENT_FREE_CB_FUNCTION' : 'MPI_T_event_free_cb_function',
    'EVENT_DROP_CB_FUNCTION' : 'MPI_T_event_dropped_cb_function',

    'STRING'              : 'char', # The '*' is added in bindingc.py
    'STRING_ARRAY'        : 'char',
    'STRING_2DARRAY'      : 'char',

    'ARGUMENT_COUNT'      : 'int',
    'ARGUMENT_LIST'       : 'char',

    # Various types of integers
    'ARRAY_LENGTH'        : 'int',
    'ARRAY_LENGTH_NNI'    : 'int',
    'ARRAY_LENGTH_PI'     : 'int',
    'ATTRIBUTE_VAL_10'    : 'void', # From MPI-1.0
    'ATTRIBUTE_VAL'       : 'void', # Current version of MPI
    'BLOCKLENGTH'         : 'int',
    'COLOR'               : 'int',
    'COORDINATE'          : 'int',
    'COORDINATE_NNI'      : 'int',
    'DEGREE'              : 'int',
    'DIMENSION'           : 'int',
    'ENUM'                : 'int',
    'FILE_DESCRIPTOR'     : 'int',
    'KEY'                 : 'int',
    'KEYVAL'              : 'int',
    'INDEX'               : 'int',
    'LOGICAL'             : 'int',
    'LOGICAL_OPTIONAL'    : 'int',
    'LOGICAL_BOOLEAN'     : 'int',
    'MATH'                : 'int',
    'NUM_DIMS'            : 'int',
    'RANK'                : 'int',
    'RANK_NNI'            : 'int',
    'COMM_SIZE'           : 'int',
    'COMM_SIZE_PI'        : 'int',
    'STRING_LENGTH'       : 'int',
    'STRIDE_BYTES'        : 'MPI_Aint',
    'STRIDE_ELEM'         : 'int',
    'TAG'                 : 'int',
    'VERSION'             : 'int',
    'WEIGHT'              : 'int',
    'OFFSET'              : 'MPI_Offset',
    'PROFILE_LEVEL'       : 'int',
    'WINDOW_SIZE'         : 'MPI_Aint',
    'INFO_VALUE_LENGTH'   : 'int',
    'ACCESS_MODE'         : 'int',
    'UPDATE_MODE'         : 'int',
    'KEY_INDEX'           : 'int',
    'TOOLENUM_INDEX'      : 'int',
    'TOOLENUM_SIZE'       : 'int',
    'TOOL_VAR_VERBOSITY'  : 'int',
    'TOOL_VAR_VALUE'      : 'int',
    'CVAR_INDEX'          : 'int',
    'CVAR_INDEX_SPECIAL'  : 'int',
    'PVAR_INDEX'          : 'int',
    'PVAR_CLASS'          : 'int',
    'SOURCE_INDEX'        : 'int',
    'TOOLS_TICK_COUNT'    : 'MPI_Count',
    'EVENT_INDEX'         : 'int',
    'CAT_INDEX'           : 'int',
    'UPDATE_NUMBER'       : 'int',
    'DROPPED_COUNT'       : 'MPI_Count',
    'TYPECLASS_SIZE'      : 'int',
    'GENERIC_DTYPE_INT'   : 'int',
    'GENERIC_DTYPE_COUNT' : 'MPI_Count',
    'PROCESS_GRID_SIZE'   : 'int',
    'DTYPE_DISTRIBUTION'  : 'int',

    # These are special.  See note in LIS_KIND_MAP for details.
    'ALLOC_MEM_NUM_BYTES' : 'MPI_Aint',
    'PACK_EXTERNAL_SIZE'  : 'MPI_Aint',
    'WIN_ATTACH_SIZE'     : 'MPI_Aint',

    # See notes about these types in LIS_KIND_MAP.
    'DISPLACEMENT_SMALL'  : 'int',
    'DISPLACEMENT'        : 'MPI_Aint',
    'DISPLACEMENT_NNI'    : 'MPI_Aint',

    'RMA_DISPLACEMENT_SMALL': 'int',
    'RMA_DISPLACEMENT'    : 'MPI_Aint',

    'XFER_NUM_ELEM_SMALL' : 'int',
    'XFER_NUM_ELEM'       : 'MPI_Count',

    'NUM_BYTES_SMALL'     : 'int',
    'NUM_BYTES'           : 'MPI_Count',

    'NUM_BYTES_NNI_SMALL' : 'int',
    'NUM_BYTES_NNI'       : 'MPI_Count',

    # Enums
    'ERROR_CODE'          : 'int',
    'ERROR_CODE_SHOW_INTENT': 'int',
    'ERROR_CLASS'         : 'int',
    'ORDER'               : 'int',
    'THREAD_LEVEL'        : 'int',
    'COMBINER'            : 'int',
    'LOCK_TYPE'           : 'int',
    'TOOLS_ENUM'          : 'MPI_T_enum',
    'BIND_TYPE'           : 'int',
    'SOURCE_ORDERING'     : 'MPI_T_source_order',
    'CALLBACK_SAFETY'     : 'MPI_T_cb_safety',
    'VARIABLE_SCOPE'      : 'int',
    'ASSERT'              : 'int',
    'TYPECLASS'           : 'int',
    'GROUP_COMPARISON'    : 'int',
    'COMM_COMPARISON'     : 'int',
    'SPLIT_TYPE'          : 'int',
    'TOPOLOGY_TYPE'       : 'int',
    'DISTRIB_ENUM'  : 'int',

    'DISPOFFSET_SMALL'    : 'MPI_Aint',
    'DISPOFFSET'          : 'MPI_Count',


    'DTYPE_NUM_ELEM_NNI_SMALL': 'int',
    'DTYPE_NUM_ELEM_NNI'      : 'MPI_Count',


    'DTYPE_NUM_ELEM_SMALL': 'int',
    'DTYPE_NUM_ELEM'      : 'MPI_Count',


    # Polymorphic types and their corresponding non-polymorphic types.
    # Anything that is POLY* means that it has one type in <=MPI-3.1
    # and a different type in >=MPI-4.0.
    'POLYDISPLACEMENT'    : None,
    'POLYRMA_DISPLACEMENT': None,
    'POLYDISPOFFSET'      : None,
    'POLYDTYPE_NUM_ELEM_NNI': None,
    'POLYDTYPE_NUM_ELEM'    : None,
    'POLYDTYPE_NUM_ELEM_PI' : None,
    'POLYTOOLS_NUM_ELEM'  : None,
    'POLYNUM_BYTES'       : None,
    'POLYNUM_BYTES_NNI'   : None,
    'POLYXFER_NUM_ELEM'   : None,
    'POLYXFER_NUM_ELEM_NNI' : None,
    'POLYDTYPE_STRIDE_BYTES': None,
    'POLYDISPLACEMENT_COUNT': None,
    'POLYDISPLACEMENT_AINT_COUNT': None,
    'POLYDTYPE_PACK_SIZE'   : None,
    'POLYRMA_DISPLACEMENT_NNI'  : None,
    'POLYLOCATION'  : None,

    'DTYPE_STRIDE_BYTES_SMALL': 'MPI_Aint',
    'DTYPE_STRIDE_BYTES'      : 'MPI_Count',

    'DTYPE_NUM_ELEM_PI_SMALL': 'int',
    'DTYPE_NUM_ELEM_PI'      : 'MPI_Count',

    'DTYPE_NUM_ELEM_SMALL': 'int',
    'DTYPE_NUM_ELEM'      : 'MPI_Count',

    'TOOLS_NUM_ELEM_SMALL': 'int',
    'TOOLS_NUM_ELEM'      : 'MPI_Count',

    'XFER_NUM_ELEM_NNI_SMALL': 'int',
    'XFER_NUM_ELEM_NNI'      : 'MPI_Count',

    'DISPLACEMENT_COUNT_SMALL': 'int',
    'DISPLACEMENT_COUNT'      : 'MPI_Count',

    'DISPLACEMENT_AINT_COUNT_SMALL': 'MPI_Aint',
    'DISPLACEMENT_AINT_COUNT'      : 'MPI_Count',

    'DTYPE_PACK_SIZE_SMALL': 'MPI_Aint',
    'DTYPE_PACK_SIZE'      : 'MPI_Count',

    'RMA_DISPLACEMENT_NNI_SMALL': 'int',
    'RMA_DISPLACEMENT_NNI'      : 'MPI_Aint',

    'LOCATION_SMALL'  : 'MPI_Aint',
    'LOCATION'        : 'MPI_Count',

    'NUM_PARAM_VALUES_SMALL': 'int',
    'POLYNUM_PARAM_VALUES': None,
    'NUM_PARAM_VALUES': 'MPI_Count',

    # MPI partitioned communication
    'PARTITION'           : 'int',

    # MPI handles
    'COMMUNICATOR'        : 'MPI_Comm',
    'DATATYPE'            : 'MPI_Datatype',
    'ERRHANDLER'          : 'MPI_Errhandler',
    'FILE'                : 'MPI_File',
    'GROUP'               : 'MPI_Group',
    'INFO'                : 'MPI_Info',
    'MESSAGE'             : 'MPI_Message',
    'REQUEST'             : 'MPI_Request',
    'SESSION'             : 'MPI_Session',
    'STATUS'              : 'MPI_Status',
    'WINDOW'              : 'MPI_Win',
    'OPERATION'           : 'MPI_Op',
    'CVAR'                : 'MPI_T_cvar_handle',
    'PVAR'                : 'MPI_T_pvar_handle',
    'PVAR_SESSION'        : 'MPI_T_pvar_session',
    'EVENT_REGISTRATION'  : 'MPI_T_event_registration',
    'EVENT_INSTANCE'      : 'MPI_T_event_instance',
    'TOOL_MPI_OBJ'        : 'void',

    # Special handles (needed for handle conversion bindings)
    'F90_STATUS'          : 'MPI_Fint',
    'F08_STATUS'          : 'MPI_F08_status',

    'F90_COMM'            : 'MPI_Fint',
    'F90_DATATYPE'        : 'MPI_Fint',
    'F90_GROUP'           : 'MPI_Fint',
    'F90_REQUEST'         : 'MPI_Fint',
    'F90_FILE'            : 'MPI_Fint',
    'F90_WIN'             : 'MPI_Fint',
    'F90_OP'              : 'MPI_Fint',
    'F90_INFO'            : 'MPI_Fint',
    'F90_ERRHANDLER'      : 'MPI_Fint',
    'F90_MESSAGE'         : 'MPI_Fint',
    'F90_SESSION'         : 'MPI_Fint',

    # Special handle for VARARGS in MPI_Pcontrol
    'VARARGS'             : '...',

    # Specials for return types
    'WALL_TIME'           : 'double',
    'TICK_RESOLUTION'     : 'double',
    'NOTHING'             : 'void'
}

# These 2 maps are meant to be used.  They have types filled in for POLLY*.

SMALL_C_KIND_MAP = copy.deepcopy(BASE_C_KIND_MAP)
SMALL_C_KIND_MAP.update({
    'POLYDISPLACEMENT'    : 'int',
    'POLYRMA_DISPLACEMENT': 'int',
    'POLYRMA_DISPLACEMENT_NNI'  : 'int',
    'POLYDISPOFFSET'      : 'MPI_Aint',
    'POLYDTYPE_NUM_ELEM_NNI': 'int',
    'POLYDTYPE_NUM_ELEM'    : 'int',
    'POLYDTYPE_NUM_ELEM_PI' : 'int',
    'POLYTOOLS_NUM_ELEM'  : 'int',
    'POLYNUM_BYTES'       : 'int',
    'POLYNUM_BYTES_NNI'       : 'int',
    'POLYXFER_NUM_ELEM'   : 'int',
    'POLYXFER_NUM_ELEM_NNI' : 'int',
    'POLYDTYPE_STRIDE_BYTES': 'MPI_Aint',
    'POLYDISPLACEMENT_COUNT': 'int',
    'POLYDISPLACEMENT_AINT_COUNT': 'MPI_Aint',
    'POLYDTYPE_PACK_SIZE'   : 'MPI_Aint',
    'POLYLOCATION'  : 'MPI_Aint',
    'POLYNUM_PARAM_VALUES': 'int',
})

BIG_C_KIND_MAP = copy.deepcopy(BASE_C_KIND_MAP)
BIG_C_KIND_MAP.update({
    'POLYDISPLACEMENT'    : 'MPI_Aint',
    'POLYRMA_DISPLACEMENT': 'MPI_Aint',
    'POLYRMA_DISPLACEMENT_NNI'  : 'MPI_Aint',
    'POLYDISPOFFSET'      : 'MPI_Count',
    'POLYDTYPE_NUM_ELEM_NNI': 'MPI_Count',
    'POLYDTYPE_NUM_ELEM'    : 'MPI_Count',
    'POLYDTYPE_NUM_ELEM_PI' : 'MPI_Count',
    'POLYTOOLS_NUM_ELEM'  : 'MPI_Count',
    'POLYNUM_BYTES'       : 'MPI_Count',
    'POLYNUM_BYTES_NNI'       : 'MPI_Count',
    'POLYXFER_NUM_ELEM'   : 'MPI_Count',
    'POLYXFER_NUM_ELEM_NNI' : 'MPI_Count',
    'POLYDTYPE_STRIDE_BYTES': 'MPI_Count',
    'POLYDISPLACEMENT_COUNT': 'MPI_Count',
    'POLYDISPLACEMENT_AINT_COUNT': 'MPI_Count',
    'POLYDTYPE_PACK_SIZE'   : 'MPI_Count',
    'POLYLOCATION'  : 'MPI_Count',
    'POLYNUM_PARAM_VALUES': 'MPI_Count',
})

#------------------------------------

# This map is not to be used directly -- it is copied and used as the
# basis for multiple other kind maps that fill in proper types for the
# POLLY* types.

BASE_F90_KIND_MAP = {
    # Pointers
    'BUFFER'              : '<type>',
    'C_BUFFER'            : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'C_BUFFER2'           : '<type>',
    'C_BUFFER3'           : '<TYPE>',
    'C_BUFFER4'           : '<type>',
    'EXTRA_STATE'         : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'EXTRA_STATE2'        : 'INTEGER',
    'FUNCTION_SMALL'      : 'EXTERNAL',
    'FUNCTION'            : 'EXTERNAL',
    'POLYFUNCTION'        : 'EXTERNAL',

    # Callback-functions for MPI_T events
    'EVENT_CB_FUNCTION'      : None,
    'EVENT_FREE_CB_FUNCTION' : None,
    'EVENT_DROP_CB_FUNCTION' : None,

    'STRING'              : 'CHARACTER*(*)',
    'STRING_ARRAY'        : 'CHARACTER*(*)',
    'STRING_2DARRAY'      : 'CHARACTER*(*)',

    'ARGUMENT_COUNT'      : None,
    'ARGUMENT_LIST'       : None,

    # Various types of integers
    'ARRAY_LENGTH'        : 'INTEGER',
    'ARRAY_LENGTH_NNI'    : 'INTEGER',
    'ARRAY_LENGTH_PI'     : 'INTEGER',
    'ATTRIBUTE_VAL_10'    : 'INTEGER', # From MPI-1.0
    'ATTRIBUTE_VAL'       : 'INTEGER(KIND=MPI_ADDRESS_KIND)', # Current version of MPI
    'BLOCKLENGTH'         : 'INTEGER',
    'COLOR'               : 'INTEGER',
    'COORDINATE'          : 'INTEGER',
    'COORDINATE_NNI'      : 'INTEGER',
    'DEGREE'              : 'INTEGER',
    'DIMENSION'           : 'INTEGER',
    'ENUM'                : 'INTEGER',
    'FILE_DESCRIPTOR'     : 'INTEGER',
    'KEY'                 : 'INTEGER',
    'KEYVAL'              : 'INTEGER',
    'INDEX'               : 'INTEGER',
    'LOGICAL'             : 'LOGICAL',
    'LOGICAL_OPTIONAL'    : None,
    'LOGICAL_BOOLEAN'     : 'LOGICAL',
    'MATH'                : 'INTEGER',
    'NUM_DIMS'            : 'INTEGER',
    'RANK'                : 'INTEGER',
    'RANK_NNI'            : 'INTEGER',
    'COMM_SIZE'           : 'INTEGER',
    'COMM_SIZE_PI'        : 'INTEGER',
    'STRING_LENGTH'       : 'INTEGER',
    'STRIDE_BYTES'        : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'STRIDE_ELEM'         : 'INTEGER',
    'TAG'                 : 'INTEGER',
    'VERSION'             : 'INTEGER',
    'WEIGHT'              : 'INTEGER',
    'OFFSET'              : 'INTEGER(KIND=MPI_OFFSET_KIND)',
    'PROFILE_LEVEL'       : 'INTEGER',
    'WINDOW_SIZE'         : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'INFO_VALUE_LENGTH'   : 'INTEGER',
    'ACCESS_MODE'         : 'INTEGER',
    'UPDATE_MODE'         : 'INTEGER',
    'KEY_INDEX'           : 'INTEGER',
    'TOOLENUM_INDEX'      : None,
    'TOOLENUM_SIZE'       : None,
    'TOOL_VAR_VERBOSITY'  : None,
    'TOOL_VAR_VALUE'      : None,
    'CVAR_INDEX'          : None,
    'CVAR_INDEX_SPECIAL'  : None,
    'PVAR_INDEX'          : None,
    'PVAR_CLASS'          : None,
    'SOURCE_INDEX'        : None,
    'TOOLS_TICK_COUNT'    : None,
    'EVENT_INDEX'         : None,
    'CAT_INDEX'           : None,
    'UPDATE_NUMBER'       : None,
    'DROPPED_COUNT'       : None,
    'TYPECLASS_SIZE'      : 'INTEGER',
    'GENERIC_DTYPE_INT'   : 'INTEGER',
    'GENERIC_DTYPE_COUNT' : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'PROCESS_GRID_SIZE'   : 'INTEGER',
    'DTYPE_DISTRIBUTION'  : 'INTEGER',

    # MPI partitioned communication
	'PARTITION'           : 'INTEGER',

    # These are special.  See note in LIS_KIND_MAP for details.
    'ALLOC_MEM_NUM_BYTES' : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'PACK_EXTERNAL_SIZE'  : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'WIN_ATTACH_SIZE'     : 'INTEGER(KIND=MPI_ADDRESS_KIND)',

    # See notes about these types in LIS_KIND_MAP.
    'DISPLACEMENT_SMALL'  : 'INTEGER',
    'DISPLACEMENT'        : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'DISPLACEMENT_NNI'    : 'INTEGER(KIND=MPI_ADDRESS_KIND)',

    'RMA_DISPLACEMENT_SMALL': 'INTEGER',
    'RMA_DISPLACEMENT'    : 'INTEGER(KIND=MPI_ADDRESS_KIND)',

    'XFER_NUM_ELEM_SMALL' : 'INTEGER',
    'XFER_NUM_ELEM'       : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'NUM_BYTES_SMALL'     : 'INTEGER',
    'NUM_BYTES'           : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'NUM_BYTES_NNI_SMALL'     : 'INTEGER',
    'NUM_BYTES_NNI'           : 'INTEGER(KIND=MPI_COUNT_KIND)',

    # Enums
    'ERROR_CODE'          : 'INTEGER',
    'ERROR_CODE_SHOW_INTENT': 'INTEGER',
    'ERROR_CLASS'         : 'INTEGER',
    'ORDER'               : 'INTEGER',
    'THREAD_LEVEL'        : 'INTEGER',
    'COMBINER'            : 'INTEGER',
    'LOCK_TYPE'           : 'INTEGER',
    'TOOLS_ENUM'          : None,
    'BIND_TYPE'           : None,
    'SOURCE_ORDERING'     : None,
    'CALLBACK_SAFETY'     : None,
    'VARIABLE_SCOPE'      : None,
    'ASSERT'              : 'INTEGER',
    'TYPECLASS'           : 'INTEGER',
    'GROUP_COMPARISON'    : 'INTEGER',
    'COMM_COMPARISON'     : 'INTEGER',
    'SPLIT_TYPE'          : 'INTEGER',
    'TOPOLOGY_TYPE'       : 'INTEGER',
    'DISTRIB_ENUM'        : 'INTEGER',

    # Polymorphic types and their corresponding non-polymorphic types.
    # Anything that is POLY* means that it has one type in <=MPI-3.1
    # and a different type in >=MPI-4.0.
    'POLYDISPLACEMENT'    : None,
    'POLYRMA_DISPLACEMENT': None,
    'POLYRMA_DISPLACEMENT_NNI'  : None,
    'POLYDISPOFFSET'      : None,
    'POLYDTYPE_NUM_ELEM_NNI': None,
    'POLYDTYPE_NUM_ELEM'    : None,
    'POLYDTYPE_NUM_ELEM_PI' : None,
    'POLYTOOLS_NUM_ELEM'  : None,
    'POLYNUM_BYTES'       : None,
    'POLYNUM_BYTES_NNI'   : None,
    'POLYXFER_NUM_ELEM'   : None,
    'POLYXFER_NUM_ELEM_NNI' : None,
    'POLYDISPLACEMENT_COUNT': None,
    'POLYDISPLACEMENT_AINT_COUNT': None,
    'POLYDTYPE_PACK_SIZE'   : None,
    'POLYDTYPE_STRIDE_BYTES': None,
    'POLYLOCATION'          : None,

    'DTYPE_STRIDE_BYTES_SMALL': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'DTYPE_STRIDE_BYTES'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DTYPE_NUM_ELEM_PI_SMALL': 'INTEGER',
    'DTYPE_NUM_ELEM_PI'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'TOOLS_NUM_ELEM_SMALL': 'INTEGER',
    'TOOLS_NUM_ELEM'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'XFER_NUM_ELEM_NNI_SMALL': 'INTEGER',
    'XFER_NUM_ELEM_NNI'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DISPLACEMENT_COUNT_SMALL': 'INTEGER',
    'DISPLACEMENT_COUNT'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DISPLACEMENT_AINT_COUNT_SMALL': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'DISPLACEMENT_AINT_COUNT'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DTYPE_PACK_SIZE_SMALL': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'DTYPE_PACK_SIZE'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DISPOFFSET_SMALL'    : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'DISPOFFSET'          : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DTYPE_NUM_ELEM_NNI_SMALL': 'INTEGER',
    'DTYPE_NUM_ELEM_NNI'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'DTYPE_NUM_ELEM_SMALL': 'INTEGER',
    'DTYPE_NUM_ELEM'      : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'RMA_DISPLACEMENT_NNI_SMALL': 'INTEGER',
    'RMA_DISPLACEMENT_NNI'      : 'INTEGER(KIND=MPI_ADDRESS_KIND)',

    'LOCATION_SMALL'  : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'LOCATION'        : 'INTEGER(KIND=MPI_COUNT_KIND)',

    'NUM_PARAM_VALUES_SMALL': 'INTEGER',
    'POLYNUM_PARAM_VALUES': None,
    'NUM_PARAM_VALUES': 'INTEGER(KIND=MPI_COUNT_KIND)',

    # MPI handles
    'COMMUNICATOR'        : 'INTEGER',
    'DATATYPE'            : 'INTEGER',
    'ERRHANDLER'          : 'INTEGER',
    'INFO'                : 'INTEGER',
    'FILE'                : 'INTEGER',
    'GROUP'               : 'INTEGER',
    'MESSAGE'             : 'INTEGER',
    'REQUEST'             : 'INTEGER',
    'SESSION'             : 'INTEGER',
    'STATUS'              : 'INTEGER',
    'WINDOW'              : 'INTEGER',
    'OPERATION'           : 'INTEGER',
    'CVAR'                : None,
    'PVAR'                : None,
    'PVAR_SESSION'        : None,
    'EVENT_REGISTRATION'  : None,
    'EVENT_INSTANCE'      : None,
    'TOOL_MPI_OBJ'        : None,

    # Special handles (needed for handle conversion bindings)
    'F90_STATUS'          : 'INTEGER', # Array size will be aded in
                                       # bindingf*.py
    'F08_STATUS'          : 'TYPE(MPI_Status)',

    'F90_COMM'            : None,
    'F90_DATATYPE'        : None,
    'F90_GROUP'           : None,
    'F90_REQUEST'         : None,
    'F90_FILE'            : None,
    'F90_WIN'             : None,
    'F90_OP'              : None,
    'F90_INFO'            : None,
    'F90_ERRHANDLER'      : None,
    'F90_MESSAGE'         : None,
    'F90_SESSION'         : None,

    # Special handle for VARARGS in MPI_Pcontrol
    'VARARGS'             : None,

    # Specials for return types
    'WALL_TIME'           : 'DOUBLE PRECISION',
    'TICK_RESOLUTION'     : 'DOUBLE PRECISION',
    'NOTHING'             : None
}

# These 2 maps are meant to be used.  They have types filled in for POLY*.

SMALL_F90_KIND_MAP = copy.deepcopy(BASE_F90_KIND_MAP)
SMALL_F90_KIND_MAP.update({
    'POLYDISPLACEMENT'      : 'INTEGER',
    'POLYRMA_DISPLACEMENT'  : 'INTEGER',
    'POLYRMA_DISPLACEMENT_NNI': 'INTEGER',
    'POLYDISPOFFSET'        : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDTYPE_NUM_ELEM_NNI': 'INTEGER',
    'POLYDTYPE_NUM_ELEM'    : 'INTEGER',
    'POLYDTYPE_NUM_ELEM_PI' : 'INTEGER',
    'POLYTOOLS_NUM_ELEM'    : 'INTEGER',
    'POLYNUM_BYTES'         : 'INTEGER',
    'POLYNUM_BYTES_NNI'     : 'INTEGER',
    'POLYXFER_NUM_ELEM'     : 'INTEGER',
    'POLYXFER_NUM_ELEM_NNI' : 'INTEGER',
    'POLYDTYPE_STRIDE_BYTES': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDISPLACEMENT_COUNT': 'INTEGER',
    'POLYDISPLACEMENT_AINT_COUNT': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDTYPE_PACK_SIZE'   : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYLOCATION'          : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYNUM_PARAM_VALUES': 'INTEGER',
})

BIG_F90_KIND_MAP = copy.deepcopy(BASE_F90_KIND_MAP)
BIG_F90_KIND_MAP.update({
    'POLYDISPLACEMENT'      : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYRMA_DISPLACEMENT'  : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYRMA_DISPLACEMENT_NNI': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDISPOFFSET'        : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_NNI': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM'    : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_PI' : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYTOOLS_NUM_ELEM'    : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES'         : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES_NNI'     : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM'     : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM_NNI' : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_STRIDE_BYTES': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_COUNT': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_AINT_COUNT': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_PACK_SIZE'   : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYLOCATION'          : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_PARAM_VALUES': 'INTEGER(KIND=MPI_COUNT_KIND)',
})

#------------------------------------

# To avoid a bit of code duplication, just copy the F90 base map into
# the F08 base map, and then modify just the handles to be the correct
# F08 types.  But just like above, this map is not to be used directly
# -- it is copied and used as the basis for multiple other kind maps
# that fill in proper types for the POLLY* types.

BASE_F08_KIND_MAP = copy.deepcopy(BASE_F90_KIND_MAP)
BASE_F08_KIND_MAP.update({
    'BUFFER'              : 'TYPE(*), DIMENSION(..)',
    'C_BUFFER'            : 'TYPE(C_PTR)',
    'C_BUFFER2'           : 'TYPE(C_PTR)',
    'C_BUFFER3'           : 'TYPE(C_PTR), VALUE',
    'C_BUFFER4'           : 'TYPE(C_PTR), VALUE',
    'FUNCTION_SMALL'      : 'PROCEDURE',  # The (type) clause is added in bindingf08.py
    'FUNCTION'            : 'PROCEDURE',
    'POLYFUNCTION'        : 'PROCEDURE',
    'STRING'              : 'CHARACTER',  # The (len) clause is added in bindingf08.py
    'STRING_ARRAY'        : 'CHARACTER',  # The (len) clause is added in bindingf08.py
    'STRING_2DARRAY'      : 'CHARACTER',  # The (len) clause is added in bindingf08.py

    'COMMUNICATOR'        : 'TYPE(MPI_Comm)',
    'DATATYPE'            : 'TYPE(MPI_Datatype)',
    'ERRHANDLER'          : 'TYPE(MPI_Errhandler)',
    'FILE'                : 'TYPE(MPI_File)',
    'GROUP'               : 'TYPE(MPI_Group)',
    'INFO'                : 'TYPE(MPI_Info)',
    'MESSAGE'             : 'TYPE(MPI_Message)',
    'REQUEST'             : 'TYPE(MPI_Request)',
    'SESSION'             : 'TYPE(MPI_Session)',
    'STATUS'              : 'TYPE(MPI_Status)',
    'WINDOW'              : 'TYPE(MPI_Win)',
    'OPERATION'           : 'TYPE(MPI_Op)',
})


# These 2 maps are meant to be used.  They have types filled in for POLY*.

SMALL_F08_KIND_MAP = copy.deepcopy(BASE_F08_KIND_MAP)
SMALL_F08_KIND_MAP.update({
    'POLYDISPLACEMENT'      : 'INTEGER',
    'POLYRMA_DISPLACEMENT'  : 'INTEGER',
    'POLYRMA_DISPLACEMENT_NNI': 'INTEGER',
    'POLYDISPOFFSET'        : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDTYPE_NUM_ELEM_NNI': 'INTEGER',
    'POLYDTYPE_NUM_ELEM'    : 'INTEGER',
    'POLYDTYPE_NUM_ELEM_PI' : 'INTEGER',
    'POLYTOOLS_NUM_ELEM'    : 'INTEGER',
    'POLYNUM_BYTES'         : 'INTEGER',
    'POLYNUM_BYTES_NNI'     : 'INTEGER',
    'POLYXFER_NUM_ELEM'     : 'INTEGER',
    'POLYXFER_NUM_ELEM_NNI' : 'INTEGER',
    'POLYDTYPE_STRIDE_BYTES': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDISPLACEMENT_COUNT': 'INTEGER',
    'POLYDISPLACEMENT_AINT_COUNT': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDTYPE_PACK_SIZE'   : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYLOCATION'          : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYNUM_PARAM_VALUES': 'INTEGER',
})

BIG_F08_KIND_MAP = copy.deepcopy(BASE_F08_KIND_MAP)
BIG_F08_KIND_MAP.update({
    'POLYDISPLACEMENT'      : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYRMA_DISPLACEMENT'  : 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYRMA_DISPLACEMENT_NNI': 'INTEGER(KIND=MPI_ADDRESS_KIND)',
    'POLYDISPOFFSET'        : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_NNI': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM'    : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_PI' : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYTOOLS_NUM_ELEM'    : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES'         : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES_NNI'     : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM'     : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM_NNI' : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_STRIDE_BYTES': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_COUNT': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_AINT_COUNT': 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_PACK_SIZE'   : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYLOCATION'          : 'INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_PARAM_VALUES': 'INTEGER(KIND=MPI_COUNT_KIND)',
})

OR_F08_KIND_MAP = copy.deepcopy(BASE_F08_KIND_MAP)
OR_F08_KIND_MAP.update({
    'POLYDISPLACEMENT'    : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYRMA_DISPLACEMENT': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPOFFSET'      : 'INTEGER(KIND=MPI_ADDRESS_KIND \\emph{or} KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_NNI': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM'    : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_NUM_ELEM_PI' : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYTOOLS_NUM_ELEM'  : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES'       : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYNUM_BYTES_NNI'       : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM'   : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYXFER_NUM_ELEM_NNI' : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_STRIDE_BYTES': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_COUNT': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDISPLACEMENT_AINT_COUNT': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYDTYPE_PACK_SIZE'   : 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
    'POLYLOCATION'          : 'INTEGER(KIND=MPI_ADDRESS_KIND \\emph{or} KIND=MPI_COUNT_KIND)',
    'POLYNUM_PARAM_VALUES': 'INTEGER \\emph{or} INTEGER(KIND=MPI_COUNT_KIND)',
})
