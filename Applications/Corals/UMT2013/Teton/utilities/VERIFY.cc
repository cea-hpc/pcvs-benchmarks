#include "utilities/VERIFY.hh"

#ifdef TAU_INSTR_DBC
TauGroup_t var_tau_gr = TAU_USER;
FunctionInfo *var_fi = NULL;

FunctionInfo* get_FI(char* name,char* filename,int lineNumber)
{
    std::string fileString(filename);
    int lastSep = fileString.find_last_of("/")+1;
    std::string reducedFile(fileString,lastSep, fileString.length()-lastSep);
    std::string label(name);
    
    std::stringstream info;
    info <<label<<"-"<<reducedFile<<"-Line:"<<lineNumber<<std::ends;

    std::vector<FunctionInfo*>& tauFunctionDB = TheFunctionDB();
    
    std::vector<FunctionInfo*>::iterator it;
    FunctionInfo* theFunctionInfoPtr=NULL;

    for(it = tauFunctionDB.begin(); it != tauFunctionDB.end(); ++it)
    {
        if( (*it)->Name == info.str() )
        {
            theFunctionInfoPtr = *it;
            break;
        }
    }
    if( theFunctionInfoPtr == NULL )
    {
        theFunctionInfoPtr = new FunctionInfo(info.str(),"",var_tau_gr,"TAU_USER");
    }
    
    var_fi = theFunctionInfoPtr;
    return var_fi;
}

#endif


namespace dbc
{

VERIFYError::VERIFYError (const char* text) {
   mReason = std::string(text);
}


}

