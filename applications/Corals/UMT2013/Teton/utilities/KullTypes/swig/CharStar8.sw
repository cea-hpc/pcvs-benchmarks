#ifndef __SWIG_CharStar8_
#define __SWIG_CharStar8_

%{
#include "utilities/KullTypes/CharStar8.hh"
#include <string>
%}

%typemap(in) CharStar8 {

if (PyString_Check($input)){
   char* temp = PyString_AsString($input);
   int len = strlen(temp);
   if(len > 8){
      PyErr_SetString(PyExc_TypeError, "string length > 8");
      return NULL;
   }
   else {
      strncpy($1.data , temp, len );
      // Pad the data with spaces, if we are under 8
      for (int i = len; i < 8; ++i){
         $1.data[i] = ' ';
      }
   }     
}
else{
   PyErr_SetString(PyExc_TypeError, "string expected");  
}
}

%typemap(out) CharStar8{
  char temp[9];
  strncpy(temp, $1.data, 8);
  temp[8] = 0;
  $result = PyString_FromString(temp);
}

%typemap(typecheck) CharStar8 {

if (PyString_Check($input)){
   char* temp = PyString_AsString($input);
   if(strlen(temp) > 8)
      $1 = 0; 
   else
      $1 = 1; 
}
else{
   $1 = 0;
}
}
#endif
