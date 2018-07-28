#ifndef MISC_H
#define MISC_H

#include <Rcpp.h>

Rcpp::IntegerMatrix
  sampleNRM(Rcpp::NumericVector theta, Rcpp::NumericVector b, Rcpp::IntegerVector a, Rcpp::IntegerVector first, Rcpp::IntegerVector last)
