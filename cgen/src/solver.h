#ifndef __SOLVER_H__
#define __SOLVER_H__


/* a structure that describes the shape of a matrix */
typedef struct matrix_shape_t {
  int domain;
  int codomain;
} matrix_shape_t;

/* a structure that describes a solver input */
typedef struct input_desc_t {
  /* what is the total size of this input
   */
  int (*size)(const int* const params);
  /* how many levels of structure does this input have?
   * e.g. 0 for a single matrix, 1 for an array of matrices, etc.
   */
  int order;
  /* what is the size of the array at this particular index into
   * the structure?  Requires 0 <= [array index] < order, and that
   * idxs is an array of length [array index].
   */
  int (**structure)(const int* const params, const int* const idxs);
  /* what is the shape of the matrix at this particular index
   * in the structure?  idxs must be an array of length order.
   */
  matrix_shape_t (*shape)(const int* const params, const int* const idxs);
} input_desc_t;

/* a structure that describes a solution */
typedef struct solution_t {
  int num_iterations;
} solution_t;

/* a structure that describes a solver */
typedef struct solver_t {
  /* the number of integer parameters passed to the solver */
  int num_params;
  /* the number of matrix-valued inputs passed to the solver */
  int num_inputs;
  /* an array of length num_inputs that describes each input */
  input_desc_t** input_descs;
  /* a function that, given the parameters, returns the size of
   * the bound variables for this problem */
  int (*variable_size)(const int* const params);
  /* the actual solver function */
  solution_t (*solve)(const int* const params, const double* const* const inputs, double* const output, double tolerance);
} solver_t;

#endif
