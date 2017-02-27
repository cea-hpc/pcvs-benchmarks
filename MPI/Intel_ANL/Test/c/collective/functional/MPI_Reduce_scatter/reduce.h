
/*********   Some definitions specific to the reduce family of functions ****/


#define OP_ARRAY_SIZE 12

struct MPITEST_op 
{
  int datatype;
  int has_op[ OP_ARRAY_SIZE];
};





/**********  function prototypes  *****************/

long apply_int_op( int, long, long);
double apply_double_op( int, double, double);
int get_reduce_answer( int op, int nump, struct dataTemplate *answer);
int has_op(int , int);
int set_default_ops(MPI_Op *);
