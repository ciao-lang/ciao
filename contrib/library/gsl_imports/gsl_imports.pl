:- module(_, _, [foreign_interface, assertions, nortchecks]).

:- include(library(gsl_imports/gsl_imports_decl_auto)).

:- foreign_inline("

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_errno.h>

#include <ciao_prolog.h>

").

% ----------------------------------------------------------------------------

:- test properties_to_matrix(Elements, Length, Rows, Cols, Matrix) :
	(Elements = [1.0, 2, 3, 4, 5, 6], Length = 6, Rows = 2, Cols = 3) =>
	( matrix_to_properties(Matrix, ElementsR, LengthR, RowsR, ColsR),
	    ElementsR = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0], LengthR = 6, RowsR = 2,
	    ColsR = 3, matrix_destroy(Matrix) )
# "Is the inverse of matrix_to_properties.".

:- true pred properties_to_matrix(in(Elements), in(Length), in(Rows),
	    in(Cols), go(Matrix)) :: c_double_list * c_size * c_size * c_size * address +
	(foreign, size_of(Elements, Length), returns(Matrix)) -->
"
gsl_matrix * properties_to_matrix(double *elements, size_t length, size_t rows, size_t cols)
{
  size_t i; /*, l = rows * cols;*/
  gsl_matrix *qt = gsl_matrix_alloc(rows, cols);
  for(i = 0; i < length; i++)
    qt->data[i] = elements[i];
  return qt;
}
".

:- true pred matrix_to_properties(in(Matrix), go(Elements), go(Length),
	    go(Rows), go(Cols)) :: address * c_double_list * c_size * c_size * c_size + (
	    foreign, size_of(Elements, Length)) -->
"
void matrix_to_properties(gsl_matrix *m, double ** list, size_t *length,
  size_t *rows, size_t *cols)
{
  size_t i;
  *length = m -> block -> size;
  /* We should reuse m -> elements in list due to it is not automatically */
  /* deallocated */
  *list = m -> block -> data;
  *rows = m -> size1;
  *cols = m -> size2;
}
".

:- true pred list_to_vector(in(Elements), in(Length), go(Vector)) ::
	c_double_list * c_size * address +
	(foreign, size_of(Elements, Length), returns(Vector)) -->
"
gsl_vector * list_to_vector(double * elements, size_t length)
{
  size_t i;
  gsl_vector *qt = gsl_vector_alloc(length);
  for(i = 0; i < length; i++)
    qt->data[i] = elements[i];
  return qt;
}
".

:- true pred vector_to_list(in(Vector), go(Elements), go(Length)) ::
	address * c_double_list * c_size + (foreign, size_of(Elements, Length)) -->
"
void vector_to_list(gsl_vector *v, double ** list, size_t *length)
{
  size_t i;
  *length = v->size;
  *list = v->data;
}
".

:- test list_to_vector(Elements, Length, Vector) :
	(Elements = [1.0, 2, 3, 4, 5, 6], Length = 6) =>
	( vector_to_list(Vector, ElementsR, LengthR),
	    ElementsR = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0], LengthR = 6,
	    vector_destroy(Vector) )
# "Is the inverse of vector_to_list.".

:- true pred matrix_destroy(in(Matrix)) :: address
	+ (foreign(gsl_matrix_free)).
:- true pred vector_destroy(in(Vector)) :: address
	+ (foreign(gsl_vector_free)).

:- true pred gsl_linalg_HH_svx(in(Matrix), in(Vector)) :: address *
	address + (foreign).

:- true pred gsl_linalg_QR_lssolve(in(QR), in(Tau), in(B), in(X),
	    in(Residual)) :: address * address * address * address * address +
	(foreign).

:- true pred gsl_linalg_QR_decomp(in(A), in(Tau)) :: address * address
	+ (foreign).

:- true pred gsl_vector_alloc(in(Size), go(Vector)) :: int * address +
	(foreign, returns(Vector)).

:- true pred gsl_matrix_alloc(in(Rows), in(Cols), go(Matrix)) :: int *
	int * address + (foreign, returns(Matrix)).

:- true pred gsl_linalg_QR_unpack(in(QR), in(Tau), in(Q), in(R)) ::
	address * address * address * address + (foreign).

:- true pred gsl_linalg_QR_U(in(QR), in(U)) :: address * address +
	(foreign) -->
"
int
gsl_linalg_QR_U (const gsl_matrix * QR, gsl_matrix * R)
{
  const size_t M = QR->size1;
  const size_t N = QR->size2;

  if (R->size1 != N || R->size2 != N)
    {
      GSL_ERROR (\"R matrix must be N x N\", GSL_ENOTSQR);
    }
  else
    {
      size_t i, j;

      /*  Form the right triangular matrix R from a packed QR matrix */

      for (i = 0; i < N; i++)
        {
          for (j = 0; j < i; j++)
            gsl_matrix_set (R, i, j, 0.0);

          for (j = i; j < N; j++)
            gsl_matrix_set (R, i, j, gsl_matrix_get (QR, i, j));
        }

      return GSL_SUCCESS;
    }
}
".

:- true pred gsl_linalg_QR_Rsvx(in(R), in(X)) :: address *
	address + (foreign).

% The next is equal to gsl_linalg_R_solve:
% :- true pred gsl_linalg_QR_Rsolve(in(QR), in(X)) :: address * address
%    + (foreign).

% But this is what I want:

:- true pred gsl_linalg_QR_Rsolve_over_determined(in(QR), in(X)) ::
	address * address + (foreign) -->
"
int
gsl_linalg_QR_Rsolve_over_determined (const gsl_matrix * QR,
  gsl_vector * x)
{
  const size_t M = QR->size1;
  const size_t N = QR->size2;

  if (M < N)
    {
      GSL_ERROR (\"QR matrix must have M>=N\", GSL_EBADLEN);
    }
  else if (N != x->size)
    {
      GSL_ERROR (\"matrix size must match solution size\", GSL_EBADLEN);
    }
  else
    {
      gsl_matrix_const_view R = gsl_matrix_const_submatrix (QR, 0, 0, N, N);

      /* Solve R x = rhs */

      gsl_blas_dtrsv (CblasUpper, CblasNoTrans, CblasNonUnit, &(R.matrix), x);

      return GSL_SUCCESS;
    }
}
".

%:- initialization(gsl_set_error_handler_off(_)).

:- true pred gsl_set_error_handler_off(go(ErrorHandler)) :: address +
	(foreign, returns(ErrorHandler)).


:- true pred gsl_version(go(VersionStr)) :: string + ( foreign(get_gsl_version),
	    do_not_free(VersionStr) ) -->

"
#include <gsl/gsl_version.h>

void
get_gsl_version(char ** ptr)
{
	*ptr = (char *) GSL_VERSION;
}
".

:- doc(doinclude, polynomial_root/5).
:- true pred polynomial_root(in(LengthIn),in(LengthOut),in(X),go(Y), go(Err))::
	c_size*c_size*c_double_list * c_double_list * int + (foreign,size_of(X,LengthIn),size_of(Y,LengthOut)) #
 "obtains roots of a polynomial function by calling foreign C program which will call GSL solver.  @var{Err} is
 error code, 0 when GSL succeed, -1 otherwise" -->
"
/* ----------------------------------------------------------------------------
   Calling GSL library for computing the roots of polynom. Polynom given as
   input_list, and output_list will contains all root of the polynom, real and 
   complex number.
   -------------------------------------------------------------------------- */
void polynomial_root(size_t nbElmt,size_t nbElmtOut,double* input_list, double** output_list, int* errcode){
  size_t i, nb_elmt_output;
  int gsl_retval;	
  gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc (nbElmt);
  *output_list=(double*) ciao_malloc ((nbElmtOut)* sizeof(double)); //this ciao_malloc will be freed by ciao prolog memory memory management system

  gsl_retval=gsl_poly_complex_solve (input_list, nbElmt, w, *output_list);     
  if (gsl_retval == GSL_EFAILED) {
    /* marking for GSL unconvergence */
    (*errcode) = -1;
  }else{
    (*errcode) = 0;
  }
  gsl_poly_complex_workspace_free (w);
}
".
