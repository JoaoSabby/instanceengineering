#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>

static void require_real_matrix(SEXP x, const char *name) {
  if (!isReal(x) || !isMatrix(x)) {
    error("'%s' deve ser uma matrix double", name);
  }
}

SEXP OU_ComputeZScoreParamsC(SEXP xMatrix) {
  require_real_matrix(xMatrix, "xMatrix");
  SEXP dims = getAttrib(xMatrix, R_DimSymbol);
  const int n = INTEGER(dims)[0];
  const int p = INTEGER(dims)[1];
  if (n < 2) {
    error("'xMatrix' deve conter ao menos duas linhas");
  }

  SEXP centers = PROTECT(allocVector(REALSXP, p));
  SEXP scales = PROTECT(allocVector(REALSXP, p));
  const double *x = REAL(xMatrix);
  double *mu = REAL(centers);
  double *sd = REAL(scales);

  for (int j = 0; j < p; ++j) {
    const double *col = x + ((R_xlen_t) j * n);
    long double sum = 0.0L;
    for (int i = 0; i < n; ++i) {
      sum += (long double) col[i];
    }
    const long double mean = sum / (long double) n;
    mu[j] = (double) mean;

    long double ss = 0.0L;
    for (int i = 0; i < n; ++i) {
      const long double centered = (long double) col[i] - mean;
      ss += centered * centered;
    }
    sd[j] = sqrt((double) (ss / (long double) (n - 1)));
  }

  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("centers"));
  SET_STRING_ELT(names, 1, mkChar("scales"));

  SEXP out = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, centers);
  SET_VECTOR_ELT(out, 1, scales);
  setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(4);
  return out;
}

SEXP OU_ApplyZScoreC(SEXP xMatrix, SEXP centers, SEXP scales, SEXP reverse) {
  require_real_matrix(xMatrix, "xMatrix");
  if (!isReal(centers) || !isReal(scales)) {
    error("'centers' e 'scales' devem ser vetores double");
  }

  SEXP dims = getAttrib(xMatrix, R_DimSymbol);
  const int n = INTEGER(dims)[0];
  const int p = INTEGER(dims)[1];
  if (XLENGTH(centers) != p || XLENGTH(scales) != p) {
    error("'centers' e 'scales' devem ter comprimento igual ao numero de colunas");
  }

  SEXP out = PROTECT(allocMatrix(REALSXP, n, p));
  const double *x = REAL(xMatrix);
  const double *mu = REAL(centers);
  const double *sd = REAL(scales);
  double *y = REAL(out);
  const int do_reverse = asLogical(reverse);

  for (int j = 0; j < p; ++j) {
    const R_xlen_t offset = (R_xlen_t) j * n;
    const double center = mu[j];
    const double scale = sd[j];
    if (do_reverse) {
      for (int i = 0; i < n; ++i) {
        y[offset + i] = x[offset + i] * scale + center;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        y[offset + i] = (x[offset + i] - center) / scale;
      }
    }
  }

  setAttrib(out, R_DimNamesSymbol, getAttrib(xMatrix, R_DimNamesSymbol));
  UNPROTECT(1);
  return out;
}

SEXP OU_GenerateSyntheticAdasynC(SEXP minorityMatrix, SEXP minorityNeighborIndex, SEXP syntheticPerRow) {
  require_real_matrix(minorityMatrix, "minorityMatrix");
  if (!isInteger(minorityNeighborIndex) || !isMatrix(minorityNeighborIndex)) {
    error("'minorityNeighborIndex' deve ser uma matrix integer");
  }
  if (!isInteger(syntheticPerRow)) {
    error("'syntheticPerRow' deve ser um vetor integer");
  }

  SEXP minorityDims = getAttrib(minorityMatrix, R_DimSymbol);
  SEXP neighborDims = getAttrib(minorityNeighborIndex, R_DimSymbol);
  const int minorityRows = INTEGER(minorityDims)[0];
  const int colCount = INTEGER(minorityDims)[1];
  const int neighborRows = INTEGER(neighborDims)[0];
  const int neighborCount = INTEGER(neighborDims)[1];

  if (neighborRows != minorityRows || XLENGTH(syntheticPerRow) != minorityRows) {
    error("Dimensoes inconsistentes para geracao ADASYN");
  }
  if (neighborCount < 1) {
    error("'minorityNeighborIndex' deve conter ao menos uma coluna");
  }

  const double *minority = REAL(minorityMatrix);
  const int *neighbor = INTEGER(minorityNeighborIndex);
  const int *perRow = INTEGER(syntheticPerRow);

  R_xlen_t totalSynthetic = 0;
  for (int i = 0; i < minorityRows; ++i) {
    if (perRow[i] < 0) {
      error("'syntheticPerRow' nao pode conter valores negativos");
    }
    totalSynthetic += (R_xlen_t) perRow[i];
  }
  if (totalSynthetic > INT_MAX) {
    error("Numero de linhas sinteticas excede o limite suportado por matrix R");
  }

  SEXP out = PROTECT(allocMatrix(REALSXP, (int) totalSynthetic, colCount));
  double *synthetic = REAL(out);

  GetRNGstate();
  int writeRow = 0;
  for (int i = 0; i < minorityRows; ++i) {
    const int rowCount = perRow[i];
    for (int r = 0; r < rowCount; ++r) {
      int sampledNeighborColumn = (int) floor(unif_rand() * (double) neighborCount);
      if (sampledNeighborColumn >= neighborCount) {
        sampledNeighborColumn = neighborCount - 1;
      }

      const int selectedNeighborRow = neighbor[i + ((R_xlen_t) sampledNeighborColumn * minorityRows)] - 1;
      if (selectedNeighborRow < 0 || selectedNeighborRow >= minorityRows) {
        PutRNGstate();
        error("'minorityNeighborIndex' contem indice fora do intervalo");
      }

      const double weight = unif_rand();
      for (int j = 0; j < colCount; ++j) {
        const double baseValue = minority[i + ((R_xlen_t) j * minorityRows)];
        const double neighborValue = minority[selectedNeighborRow + ((R_xlen_t) j * minorityRows)];
        synthetic[writeRow + ((R_xlen_t) j * totalSynthetic)] = baseValue + weight * (neighborValue - baseValue);
      }
      ++writeRow;
    }
  }
  PutRNGstate();

  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"OU_ComputeZScoreParamsC", (DL_FUNC) &OU_ComputeZScoreParamsC, 1},
  {"OU_ApplyZScoreC", (DL_FUNC) &OU_ApplyZScoreC, 4},
  {"OU_GenerateSyntheticAdasynC", (DL_FUNC) &OU_GenerateSyntheticAdasynC, 3},
  {NULL, NULL, 0}
};

static void register_over_under_routines(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

void R_init_instanceengineering(DllInfo *dll) {
  register_over_under_routines(dll);
}

void R_init_over_under_fast(DllInfo *dll) {
  register_over_under_routines(dll);
}
