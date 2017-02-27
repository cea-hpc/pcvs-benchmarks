
/*********   Some definitions specific to the reduce family of functions ****/


#define OP_ARRAY_SIZE 10

struct MPITEST_op 
{
  int datatype;
  int has_op[ OP_ARRAY_SIZE];
};





/**********  function prototypes  *****************/

long apply_int_op( int, long, long);
double apply_double_op( int, double, double);
#if MPITEST_long_double_def
long double apply_long_double_op(int op_index, long double x1, long double x2);
#endif
int get_reduce_answer( int op, int nump, struct dataTemplate *answer);
int has_op(int , int);
int set_default_ops(MPI_Op *);
