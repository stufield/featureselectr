#include <Rcpp.h>
using namespace Rcpp;

/* Calculate MAD over columns */
/* apply(x, 2, mad, na.rm = TRUE, constant = 1.4826) */
// [[Rcpp::export]]
List run_fs_cpp(List x, Formula form) {
  List cv = x["cross_val"];
  int nrun = cv["runs"];
  int nfold = cv["folds"];
  List ret(nrun);
  Function r_cost(".cost");
  Function r_fitmodel(".fitmodel");
  for (int i = 0; i < nrun; ++i) {
    NumericVector cost_vec(nfold);
    cv["current_run"] = i+1;
    x["cross_val"] = cv;
    for (int j = 0; j < nfold; ++j) {
      cv["current_fold"] = j+1;
      x["cross_val"] = cv;
      x = r_fitmodel(x, Named("frmla") = form);
      cost_vec[j] = as<double>(r_cost(x));
    }
    ret[i] = cost_vec;
  }
  return ret;
}


/* Testing output */
/***R
x <- fs_obj
frmla <- create_form(x$model_type$response, head(x$candidate_features))
foo <- function(n = 1L) {
  parallel::mclapply(seq_len(x$cross_val$runs), function(r) {
  x$cross_val$current_run <- r  # tmp element for: .cost .fitmodel
  vapply(seq_len(x$cross_val$folds), function(f) {
    x$cross_val$current_fold <- f
    .cost(.fitmodel(x, frmla = frmla))
  }, FUN.VALUE = double(1))
  }, mc.cores = n)
}
bench::mark(memory= F,
  orig = foo(5),
  cpp = run_fs_cpp(x, frmla)
)
*/
