#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Optimized helper function to calculate the maximum value within each group
NumericVector ave_max(NumericVector x, NumericVector group) {
  int n = x.size();
  NumericVector result(n);
  
  // Pre-compute maximum values for each group
  NumericVector group_max = clone(x);
  for (int i = n - 2; i >= 0; --i) {
    if (group[i] == group[i + 1]) {
      group_max[i] = std::max(group_max[i], group_max[i + 1]);
    }
  }

  return group_max;
}

// [[Rcpp::export]]
List calc_grad_hess(NumericVector lp, NumericMatrix x, 
                              NumericVector time, NumericVector status) {
  int n_samples = time.size();
  int n_features = x.ncol();

  if (n_samples == 1) {
    return List::create(
      Named("grad") = NumericVector(n_features, 0.0),
      Named("hess") = NumericVector(n_features * n_features, 0.0)
    );
  }

  NumericVector hazard = exp(lp);
  NumericVector cumsum_hazard = cumsum(hazard);
  NumericVector risk_set = ave_max(cumsum_hazard, time);

  // Precompute hazard_x for gradient calculations
  NumericMatrix hazard_x = clone(x);
  for (int i = 0; i < n_samples; i++) {
    for (int j = 0; j < n_features; j++) {
      hazard_x(i, j) *= hazard[i];
    }
  }

  // Cumulative sum for each feature
  NumericMatrix risk_set_x = clone(hazard_x);
  for (int j = 0; j < n_features; j++) {
    NumericVector col = hazard_x(_, j);
    NumericVector cumsum_col = cumsum(col);
    risk_set_x(_, j) = ave_max(cumsum_col, time);
  }

  // Compute risk_set_x_ratio
  NumericMatrix risk_set_x_ratio(n_samples, n_features);
  for (int i = 0; i < n_samples; i++) {
    for (int j = 0; j < n_features; j++) {
      risk_set_x_ratio(i, j) = risk_set_x(i, j) / risk_set[i];
    }
  }

  // arma matrix for faster matrix operations
  mat arma_x(x.begin(), n_samples, n_features, false);
  mat hazard_xx(n_samples, n_features * n_features, fill::zeros);

  // Compute outer product for Hessian calculation
  for (int i = 0; i < n_samples; i++) {
    mat outer_product = arma_x.row(i).t() * arma_x.row(i);
    hazard_xx.row(i) = vectorise(outer_product).t() * hazard[i];
  }

  // Compute risk_set_xx_ratio
  NumericMatrix risk_set_xx_ratio(n_samples, n_features * n_features);
  for (int j = 0; j < n_features * n_features; j++) {
    NumericVector col = wrap(hazard_xx.col(j));
    NumericVector cumsum_col = cumsum(col);
    NumericVector max_col = ave_max(cumsum_col, time);
    for (int i = 0; i < n_samples; i++) {
      risk_set_xx_ratio(i, j) = max_col[i] / risk_set[i];
    }
  }

  // Gradient and Hessian initialization
  NumericMatrix grad(n_samples, n_features);
  NumericMatrix hess(n_samples, n_features * n_features);

  // Parallel loop for gradient and Hessian computation
  #pragma omp parallel for
  for (int i = 0; i < n_samples; i++) {
    for (int j = 0; j < n_features; j++) {
      grad(i, j) = (x(i, j) - risk_set_x_ratio(i, j)) * status[i];
    }
    
    for (int j = 0; j < n_features * n_features; j++) {
      hess(i, j) = risk_set_xx_ratio(i, j) * status[i];
    }
  }

  return List::create(
    Named("grad") = grad,
    Named("hess") = hess
  );
}