#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix
  sampleNRM(NumericVector theta, NumericVector b, IntegerVector a, IntegerVector first, IntegerVector last)
  {
    int nI = first.length();
    int nP = theta.length();
    IntegerMatrix x = IntegerMatrix(nP,nI);
    
    double u;
    int k, ncat;
    
    for (int i = 0; i<nI; i++)
    {
      ncat = last[i]-first[i]+2;
      NumericVector p = NumericVector(ncat);
      for (int pers = 0; pers<nP; pers++)
      {
        p[0]=1;
        k=1;
        for (int j=first[i];j<=last[i];j++)
        {
          p[k]=p[k-1]+b[j]*exp(a[j]*theta[pers]);
          k++;
        }
        u = p[k-1] * R::runif(0,1);
        k=0;
        while (u>p[k]) {k++;}
        if (k>0) {x(pers,i) = a[first[i]+k-1];}
      }
    }
    return x;
  }
