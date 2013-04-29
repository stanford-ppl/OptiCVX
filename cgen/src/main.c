
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "solver.h"

extern solver_t solver;

input_t* read_input(FILE* f, input_desc_t* desc, int* params);
input_t* read_input_sub(FILE* f, input_desc_t* desc, int* params, int n_idxs, int* idxs);
double* read_matrix(FILE* f, matrix_shape_t shape);

input_t* read_input(FILE* f, input_desc_t* desc, int* params) {
  /* allocate enough space on the stack for the temporary array */
  int tmpidxs[desc->order];
  return read_input_sub(f, desc, params, 0, tmpidxs);
}

input_t* read_input_sub(FILE* f, input_desc_t* desc, int* params, int n_idxs, int* idxs) {
  if(n_idxs == desc->order) {
    return (input_t*)read_matrix(f, desc->shape(params, idxs));
  }
  int isz = desc->structure[n_idxs](params, idxs);
  input_t* rv = malloc(isz * sizeof(input_t*));
  for(int i = 0; i < isz; i++) {
    idxs[n_idxs] = i;
    rv->idx[i] = read_input_sub(f, desc, params, n_idxs + 1, idxs);
  }
  return rv;
}

double* read_matrix(FILE* f, matrix_shape_t shape) {
  double* rv = malloc(shape.domain * shape.codomain * sizeof(double));
  for(int i = 0; i < shape.domain * shape.codomain; i++) {
    const char* scanstr;
    if(i == (shape.domain * shape.codomain - 1)) {
      if(i == 0) {
        scanstr = " [ %lf ]";
      }
      else if(i % shape.domain == 0) {
        scanstr = " ; %lf ]";
      }
      else {
        scanstr = " , %lf ]";
      }
    }
    else {
      if(i == 0) {
        scanstr = " [ %lf";
      }
      else if(i % shape.domain == 0) {
        scanstr = " ; %lf";
      }
      else {
        scanstr = " , %lf";
      }
    }
    if(fscanf(f, scanstr, &(rv[i])) == 0) {
      fprintf(stderr, "error in reading input matrix.\n");
      exit(-1);
    }
  }
  return rv;
}

int main(int argc, char** argv) {
  if (argc != solver.num_params + 2) {
    printf("error: expected %d arguments.\n", solver.num_params);
    return -1;
  }

  double tolerance = atof(argv[1]);

  int params[solver.num_params];
  for(int i = 0; i < solver.num_params; i++) {
    params[i] = atoi(argv[i+2]);
  }

  input_t* inputs[solver.num_inputs];
  for(int i = 0; i < solver.num_inputs; i++) {
    inputs[i] = read_input(stdin, solver.input_descs[i], params);
  }

  int problem_size = solver.variable_size(params);
  double output[problem_size];
  clock_t start = clock();
  solution_t solution = solver.solve(params, inputs, output, tolerance);
  double elapsed = ((double)(clock() - start))/(CLOCKS_PER_SEC);

  for(int i = 0; i < problem_size; i++) {
    printf("%g\n", output[i]);
  }

  printf("iterations: %d\n", solution.num_iterations);
  printf("time: %g\n", elapsed);

  return 0;

}

