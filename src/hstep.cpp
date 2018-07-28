#include <Rcpp.h>
#include "elmerge.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix
  H_STEP_MSC(NumericVector b, IntegerVector a, 
             List first, List last, 
             IntegerVector scoretab, 
             IntegerVector min_scores, IntegerVector max_scores,
             int routing)
  {
    int nMod = first.length();
    int ms = scoretab.length() - 1;
    int nI, nm2, lgik, lgi, lgk;
    IntegerVector m_first, m_last, m2_first, m2_last;
    NumericVector g, gi, gk, gik;
    NumericMatrix H = NumericMatrix(a.length());
    List g_list = List(nMod);
    List range_list = List(nMod);
    
    for (int m = 0; m < nMod; m++)
    {
      g_list[m] = elsym0C(b, a, first[m], last[m]);
      range_list[m] = Range(min_scores[m], max_scores[m]);
    }
    g = elsym_submergeC(g_list, range_list, routing);
    
    for (int m = 0; m < nMod; m++)
    {
      m_first = as<IntegerVector>(first[m]);
      m_last = as<IntegerVector>(last[m]);
      nI = m_first.length();
      for (int item = 0; item < nI; item++)
      {
        gi = elsym_mergeC(b, a, first, last, g_list, range_list, 
                          IntegerVector::create(m, item), 
                          IntegerVector::create(-1,-1), 
                          routing);
        lgi = gi.length();
        for (int j = m_first[item]; j <= m_last[item]; j++)
        {
          for (int s = a[j]; s < ms; s++)
          {
            if (g[s]>0)
            {
              H(j,j) += scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(1-(gi[s-a[j]]*b[j]/g[s]));
            }
          }
          // between categories of the same item
          for (int k = (j+1); k <= m_last[item]; k++)
          {
            for (int s = a[k]; s < ms; s++)
            {
              if (g[s]>0)
              {
                H(k,j) -= scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gi[s-a[k]]*b[k]/g[s]);
              }
            }
            H(j,k) = H(k,j);
          }
          // Between items in same module
          for (int k = (item + 1); k < nI; k++)
          {
            gk = elsym_mergeC(b, a, first, last, g_list, range_list, 
                              IntegerVector::create(-1,-1), IntegerVector::create(m,k), routing);
            gik = elsym_mergeC(b, a, first, last, g_list, range_list, 
                               IntegerVector::create(m, item), IntegerVector::create(m,k), routing);
            lgk = gk.length();
            lgik = gik.length();
            for (int l = m_first[k]; l <= m_last[k]; l++)
            {
              for (int s = 0; s < ms; s++)
              {
                if (g[s]>0)
                {
                  if ((s >=(a[j]+a[l]))&&((s-a[j]-a[l])<lgik))
                  {
                    H(j,l) += scoretab[s]*(gik[s-a[j]-a[l]])*((b[j]*b[l])/g[s]);
                  }
                  if ((s >= a[j])&&(s >= a[l])&&((s-a[l])<lgk)&&((s-a[j])<lgi)) {
                    H(j,l) -= scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gk[s-a[l]]*b[l]/g[s]);
                  }
                }
              }
              H(l,j) = H(j,l);
            }
          }
          
          // between items in other modules
          for (int m2 = m+1; m2 < nMod; m2++)
          {
            m2_first = as<IntegerVector>(first[m2]);
            m2_last = as<IntegerVector>(last[m2]);
            nm2 = m2_first.length();
            for (int k = 0; k < nm2; k++)
            {
              gk = elsym_mergeC(b, a, first, last, g_list, range_list, 
                                IntegerVector::create(-1,-1), IntegerVector::create(m2,k), routing);
              gik = elsym_mergeC(b, a, first, last, g_list, range_list, 
                                 IntegerVector::create(m, item), IntegerVector::create(m2,k), routing);
              lgk = gk.length();
              lgik = gik.length();
              for (int l = m2_first[k]; l <= m2_last[k]; l++)
              {
                for (int s = 0; s <= ms; s++)
                {
                  if (g[s]>0)
                  {
                    if ((s >= (a[j]+a[l]))&&((s-a[j]-a[l])<lgik)){
                      H(l,j) +=  scoretab[s]*(gik[s-a[j]-a[l]])*((b[j]*b[l])/g[s]);
                    }
                    if ((s >= a[j])&&(s >= a[l])) {
                      H(l,j) -= scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gk[s-a[l]]*b[l]/g[s]);
                    }
                  }
                }
                H(j,l) = H(l,j);
              }
            }
          }
        } //END j loop
      } //END item loop
    } //END module loop
    return H;
  }

