#include <mpc.h>
#include <stdio.h>
#include <assert.h>

static int Required_option(MPC_Config_Status_t option){
  if(MPC_Config_Status(option) == 0){
    fprintf(stderr,"Requirement %s failed\n",MPC_Config_Status_Name(option));
    return 1;
  } 
  return 0;
}

int main(int argc, char** argv){
  int res = 0; 
  MPC_Config_Status_Print(stderr);

  res +=  Required_option(MPC_HAVE_OPTION_HLS);
  res +=  Required_option(MPC_HAVE_OPTION_ETLS_COW);
  res +=  Required_option(MPC_HAVE_OPTION_ETLS_OPTIMIZED);

  assert(res == 0);

  return 0;
}
