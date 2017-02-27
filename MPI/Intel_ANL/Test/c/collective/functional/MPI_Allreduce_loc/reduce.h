
/*********   Some definitions specific to the reduce family of functions ****/


#define OP_ARRAY_SIZE 2

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

typedef struct MPITEST_float_int_struct {
	float Float;
	int   Node;}  MPITEST_float_int_struct;

typedef struct MPITEST_double_int_struct {
	double Double;
	int    Node;}  MPITEST_double_int_struct;

typedef struct MPITEST_long_int_struct {
	long  Long;
	int   Node;}  MPITEST_long_int_struct;

typedef struct MPITEST_2int_struct {
	int   Int;
	int   Node;}  MPITEST_2int_struct;

typedef struct MPITEST_short_int_struct {
	short ShortInt;
	int   Node;}  MPITEST_short_int_struct;

#if MPITEST_long_double_def
typedef struct MPITEST_long_double_int_struct {
	long double  LongDouble;
	int   Node;}  MPITEST_long_double_int_struct;
#endif
