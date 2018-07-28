
#include <Rcpp.h>
using namespace Rcpp;


enum ROUTING{ ALL = 0, LAST = 1};

template <int RTYPE>
inline Rcpp::Vector<RTYPE> 
anti_subset(const Rcpp::Vector<RTYPE>& x, Rcpp::IntegerVector idx) 
{
  Rcpp::IntegerVector xi = Rcpp::seq(0, x.size() - 1);
  return x[Rcpp::setdiff(xi, idx)];
}

// WARNING
// ALL FUNCTIONS ASSUME 
// INPUTS ARE 0-INDEXED
// (first, last, etc.)

// compute Symmetric Basis Functions 
// Warning: It is necessary that elements of a are montone nondecreasing !
// This is used in CML and should be replaced with time


// [[Rcpp::export]]
NumericVector
  elsym0C(NumericVector b, IntegerVector a, IntegerVector first, IntegerVector last)
{
  int nI = first.length();
  IntegerVector a_last = a[last];
  int mS = sum(a_last);
  NumericVector g = NumericVector(mS + 1);
  int item1 = -1, item2 = -1;
  int Msc=0, initial=0;

  g[0]=1;
  for (int j=first[initial];j<=last[initial];j++){g[a[j]]=b[j];}
  Msc=a[last[initial]];
  for (int i=1;i<nI;i++)
  {
    if ((!(i==item1))&&(!(i==item2))&&(!(i==initial)))
    {
      for (int s=Msc;s>=0;s--)
      {
        for (int j=last[i];j>=first[i];j--)
        {
          g[s+a[j]]+=g[s]*b[j];
        }
      }
      Msc+=a[last[i]];
    }
  }
  return g;
}



NumericVector
  ESF_merge_allC(NumericVector g1, IntegerVector rn1, NumericVector g2, IntegerVector rn2)
{
  int idx;
  int n1 = rn1.length(); 
  int l2 = g2.length();
  int n2 = rn2.length();
  int mS = g1.length() + g2.length() - 1;
  NumericVector out(mS); //init to 0 is automatic
    
  for (int i=0;i<n1;i++)
  {
    for (int j=0;j<n2;j++)
    {
      idx=rn2[j]-rn1[i];
      if ((idx>=0)&&(idx<l2))
      {
        out[rn2[j]] += g1[rn1[i]]*g2[idx];
      }
    }
  }
  return out;
}

NumericVector
  ESF_merge_lastC(NumericVector g1, IntegerVector rn1, NumericVector g2, IntegerVector rn2)
{
  int n1 = rn1.length(); 
  int n2 = rn2.length();
  int mS = g1.length() + g2.length() - 1;
  NumericVector out(mS); //init to 0 is automatic
  
  for (int i=0;i < n1;i++)
  {
    for (int j=0;j < n2;j++)
    {
      out[rn1[i]+rn2[j]] += g1[rn1[i]]*g2[rn2[j]];
    }
  }
  return out;
}


// [[Rcpp::export]]
NumericVector 
  elsym_submergeC(List g_list, List range_list, int routing)
{
  int n_g = g_list.length();
  
  NumericVector g = as<NumericVector>(g_list[0]);
  IntegerVector range_1 = IntegerVector::create(0);
  IntegerVector range_2;
  
  if (n_g > 1)
  {
    range_2 = as<IntegerVector>(range_list[0]);
    if(routing == LAST)
    {
      for (int m = 1; m < n_g; m++)
      {
        range_1 = Range( min(range_1) + min(range_2), max(range_1) + max(range_2) );
        range_2 = as<IntegerVector>(range_list[m]);
        g = ESF_merge_lastC(g, range_1, as<NumericVector>(g_list[m]), range_2);
      }
    }
    
    if(routing == ALL)
    {
      for (int m = 1; m < n_g; m++)
      {
        range_1 = Range( min(range_2), max(range_2) );  // to check for 0
        range_2 =  as<IntegerVector>(range_list[m]);
        g = ESF_merge_allC(g, range_1, as<NumericVector>(g_list[m]), range_2);
      }
    }
  }
  return g;
}


// [[Rcpp::export]]
NumericVector
  elsym_mergeC(NumericVector b, IntegerVector a, 
               List first, List last, 
               List g_list_, List range_list_, 
               IntegerVector mi1, IntegerVector mi2, int routing)
{
  int nMod = first.length();
  int m, item, mx_it;
  NumericVector g;
  IntegerVector m_first, m_last, m_range_list, idx;  
  List range_list = clone(range_list_);
  List g_list = clone(g_list_);
  
  if ((mi1[1] < 0) && (mi2[1] < 0))
  {
    g = elsym_submergeC(g_list, range_list, routing);
  } 
  else
  {
    if ((mi1[1] < 0) ^ (mi2[1] < 0))
    {
      if (mi2[1] < 0){
        m = mi1[0]; 
        item = mi1[1];
      }else
      {
        m = mi2[0]; 
        item = mi2[1];
      }
      m_last = as<IntegerVector>(last[m]);
      mx_it = a[m_last[item]];
      if (routing == ALL)
      {
        for (int j = m; j < nMod; j++)
        {
          m_range_list = as<IntegerVector>(range_list[j]);
          m_range_list = m_range_list - mx_it;
          m_range_list = m_range_list[m_range_list >= 0];
          range_list[j] = m_range_list;
        }
      }
      if (routing == LAST)
      {
        m_range_list = as<IntegerVector>(range_list[m]);
        m_range_list = m_range_list - mx_it;
        m_range_list = m_range_list[m_range_list >= 0];
        range_list[m] = m_range_list;
      }
      m_first = as<IntegerVector>(first[m]);
      m_last = as<IntegerVector>(last[m]);
      idx = IntegerVector::create(item);
      g_list[m] = elsym0C(b, a, anti_subset(m_first, idx), anti_subset(m_last, idx));
      
      g = elsym_submergeC(g_list, range_list, routing);
    }else 
    {
      if (mi1[0] == mi2[0]) // same module
      {
        m = mi1[0];
        m_first = as<IntegerVector>(first[m]);
        m_last = as<IntegerVector>(last[m]);
        mx_it = a[m_last[mi1[1]]] + a[m_last[mi2[1]]];
        
        if (routing == ALL)
        {
          for (int j = m; j < nMod; j++)
          {
            m_range_list = as<IntegerVector>(range_list[j]);
            m_range_list = m_range_list - mx_it;
            m_range_list = m_range_list[m_range_list >= 0];
            
            range_list[j] = m_range_list;
          }
        }
        if (routing == LAST)
        {
          m_range_list = as<IntegerVector>(range_list[m]);
          m_range_list = m_range_list - mx_it;
          m_range_list = m_range_list[m_range_list >= 0];
          
          range_list[m] = m_range_list;
        }
        m_first = as<IntegerVector>(first[m]);
        m_last = as<IntegerVector>(last[m]);
        idx = IntegerVector::create(mi1[1], mi2[1]);
        g_list[m] = elsym0C(b, a, anti_subset(m_first, idx), anti_subset(m_last, idx));
          
      } else // different modules
      {
        // first item in first module
        m = mi1[0]; item = mi1[1];
        m_last = as<IntegerVector>(last[m]);
        mx_it = a[m_last[item]];
        if (routing == ALL)
        {
          for (int j = m; j < nMod; j++)
          {
            m_range_list = as<IntegerVector>(range_list[j]);
            m_range_list = m_range_list - mx_it;
            m_range_list = m_range_list[m_range_list >= 0];
            
            range_list[j] = m_range_list;
            //Rprintf("range_list[%i][0] = %i\n", j, as<IntegerVector>(range_list[j])[0]);
          }
        }
        if (routing == LAST)
        {
          m_range_list = as<IntegerVector>(range_list[m]);
          m_range_list = m_range_list - mx_it;
          m_range_list = m_range_list[m_range_list >= 0];
          
          range_list[m] = m_range_list;
        }
        m_first = as<IntegerVector>(first[m]);
        m_last = as<IntegerVector>(last[m]);
        idx = IntegerVector::create(item);
        g_list[m] = elsym0C(b, a, anti_subset(m_first, idx), anti_subset(m_last, idx));
        
        // Second item in second module
        m = mi2[0]; item = mi2[1];
        m_last = as<IntegerVector>(last[m]); // !
        mx_it = a[m_last[item]];
        if (routing == ALL)
        {
          for (int j = m; j < nMod; j++)
          {
            m_range_list = as<IntegerVector>(range_list[j]);
            m_range_list = m_range_list - mx_it;
            m_range_list = m_range_list[m_range_list >= 0];
            
            range_list[j] = m_range_list;
            //Rprintf("range_list[%i][0] = %i\n", j, as<IntegerVector>(range_list[j])[0]);
          }
        }
        if (routing == LAST)
        {
          m_range_list = as<IntegerVector>(range_list[m]);
          m_range_list = m_range_list - mx_it;
          m_range_list = m_range_list[m_range_list >= 0];
          
          range_list[m] = m_range_list;
        }
        m_first = as<IntegerVector>(first[m]);
        m_last = as<IntegerVector>(last[m]);
        idx = IntegerVector::create(item);
        g_list[m] = elsym0C(b, a, anti_subset(m_first, idx), anti_subset(m_last, idx));
      }
      g = elsym_submergeC(g_list, range_list, routing);
    }
  }
  return g;
}

// [[Rcpp::export]]
NumericVector
  E_STEP_MSC(NumericVector b, IntegerVector a, 
             List first, List last, IntegerVector scoretab, 
             IntegerVector min_scores, IntegerVector max_scores,
             int routing)
  {
    int nMod = first.length();
    IntegerVector m_first, m_last, a_last;
    int nI, ms, idx, lgi;
    NumericVector E(b.size());
    NumericVector g, gi;
    List g_list = List(nMod);
    List range_list = List(nMod);
    
    ms=0;
    for (int m = 0; m < nMod; m++)
    {
      g_list[m] = elsym0C(b, a, first[m], last[m]);
      range_list[m] = Range(min_scores[m], max_scores[m]);
    }
    g = elsym_submergeC(g_list, range_list, routing);
    ms = g.length()-1;
   
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
          for (int s = a[j]; s <= ms; s++) 
          {
            idx = s-a[j];
            if ((g[s]>0)&&(idx<lgi)) E[j] += scoretab[s]*gi[idx]*b[j]/g[s];
          }
        }
      }
    }
    return(E);
  }



