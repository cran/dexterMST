#ifndef ELMERGE_H
#define ELMERGE_H

#include <Rcpp.h>

Rcpp::NumericVector
  elsym0C(Rcpp::NumericVector b, Rcpp::IntegerVector a, Rcpp::IntegerVector first, Rcpp::IntegerVector last);

Rcpp::NumericVector
  ESF_merge_allC(Rcpp::NumericVector g1, Rcpp::IntegerVector rn1, Rcpp::NumericVector g2, Rcpp::IntegerVector rn2);
  
Rcpp::NumericVector
    ESF_merge_lastC(Rcpp::NumericVector g1, Rcpp::IntegerVector rn1, Rcpp::NumericVector g2, Rcpp::IntegerVector rn2);

  Rcpp::NumericVector 
    elsym_submergeC(Rcpp::List g_list, Rcpp::List range_list, int routing);
    
Rcpp::NumericVector
    elsym_mergeC(Rcpp::NumericVector b, Rcpp::IntegerVector a, 
                 Rcpp::List first, Rcpp::List last, Rcpp::List g_list, Rcpp::List range_list_, 
                 Rcpp::IntegerVector mi1, Rcpp::IntegerVector mi2, int routing);
  
#endif
