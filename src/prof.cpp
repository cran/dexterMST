#include <Rcpp.h>
using namespace Rcpp;

enum ROUTING{ ALL = 0, LAST = 1};


// [[Rcpp::export]]
void prof_probC(	/* output */ NumericMatrix prob, 
				/* input */ IntegerVector min_scores, IntegerVector max_scores, 
				List gA, List gB, 
				IntegerVector MscA, IntegerVector MscB, 
				int nMod, int routing)
{
  int sa, sb, sab_1, sab_2;
  double tmp_a1, tmp_a2, tmp_a3, tmp_a4, tmp_b1, tmp_b2, tmp_b3;
  NumericVector tmpA1, tmpB1, tmpA2, tmpB2, tmpA3, tmpB3, tmpA4, tmpB4;
  
  tmpA1 = as<NumericVector>(gA[0]);
  tmpB1 = as<NumericVector>(gB[0]);

  if(nMod > 1)
  {
	tmpA2 = as<NumericVector>(gA[1]);
	tmpB2 = as<NumericVector>(gB[1]);
  }
  if(nMod > 2)
  {
	tmpA3 = as<NumericVector>(gA[2]);
	tmpB3 = as<NumericVector>(gB[2]);
  }
  if(nMod > 3)
  {
	tmpA4 = as<NumericVector>(gA[3]);
	tmpB4 = as<NumericVector>(gB[3]);
  }
  
  if (routing == LAST)
  {	
	if (nMod == 1)
	{	  
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  sa = sa_1;
		  sb = sb_1;
		  prob(sa,sb) = prob(sa,sb) + tmp_a1 * tmpB1[sb_1];
		}
	  }
	}
	
	if (nMod == 2)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  if (((sa_1+sb_1)>=min_scores[0])&&((sa_1+sb_1)<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				sa = sa_1 + sa_2;
				sb = sb_1 + sb_2;
				prob(sa,sb) = prob(sa,sb) + tmp_a1*tmp_b1*tmp_a2*tmpB2[sb_2];
			  }
			}
		  }
		}
	  }
	}// nMod ==2
	
	if (nMod == 3)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  if (((sa_1+sb_1)>=min_scores[0])&&((sa_1+sb_1)<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				tmp_b2 = tmpB2[sb_2];
				if (((sa_2+sb_2)>=min_scores[1])&&((sa_2+sb_2)<=max_scores[1]))
				{
				  for (int sa_3 = 0; sa_3 <= MscA[2]; sa_3++)
				  {
					tmp_a3 = tmpA3[sa_3];
					for (int sb_3 = 0; sb_3 <= MscB[2]; sb_3++)
					{
					  sa = sa_1 + sa_2 + sa_3;
					  sb = sb_1 + sb_2 + sb_3;
					  prob(sa,sb) = prob(sa,sb) + tmp_a1 * tmp_b1 * tmp_a2 * tmp_b2 * tmp_a3 * tmpB3[sb_3];
					}
				  }
				}
			  }
			}
		  }
		}
	  }
	}// nMod ==3
	
	if (nMod == 4)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  if (((sa_1+sb_1)>=min_scores[0])&&((sa_1+sb_1)<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				tmp_b2 = tmpB2[sb_2];
				if (((sa_2+sb_2)>=min_scores[1])&&((sa_2+sb_2)<=max_scores[1]))
				{
				  for (int sa_3 = 0; sa_3 <= MscA[2]; sa_3++)
				  {
					tmp_a3 = tmpA3[sa_3];
					for (int sb_3 = 0; sb_3 <= MscB[2]; sb_3++)
					{
					  tmp_b3 = tmpB3[sb_3];
					  if (((sa_3+sb_3)>=min_scores[2])&&((sa_3+sb_3)<=max_scores[2]))
					  {
						for (int sa_4 = 0; sa_4 <= MscA[3]; sa_4++)
						{
						  tmp_a4 = tmpA4[sa_4];
						  for (int sb_4 = 0; sb_4 <= MscB[3]; sb_4++)
						  {
							sa = sa_1 + sa_2 + sa_3 + sa_4;
							sb = sb_1 + sb_2 + sb_3 + sb_4;
							prob(sa,sb) = prob(sa,sb) + tmp_a1 * tmp_b1 * tmp_a2 * tmp_b2 * tmp_a3 * tmp_b3 * tmp_a4 * tmpB4[sb_4];
						  }
						}
					  }
					}
				  }
				}
			  }
			}
		  }
		}
	  }
	}// nMod ==4
	
  } //// routing last
  
  if (routing == ALL)
  {
	if (nMod == 1)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  sa = sa_1;
		  sb = sb_1;
		  prob(sa,sb) = prob(sa,sb) + tmp_a1*tmpB1[sb_1];
		}
	  }
	}
	
	if (nMod == 2)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  if (((sa_1+sb_1)>=min_scores[0])&&((sa_1+sb_1)<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				sa = sa_1 + sa_2;
				sb = sb_1 + sb_2;
				if (((sa+sb)>=min_scores[1])&((sa+sb)<=max_scores[1])){
				  prob(sa,sb) = prob(sa,sb) + tmp_a1 * tmp_b1 * tmp_a2 * tmpB2[sb_2];
				}
			  }
			}
		  }
		}
	  }
	}// nMod ==2
	
	if (nMod == 3)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  sab_1 = sa_1 + sb_1;
		  //Rprintf( "%i", sb_1);
		  if ((sab_1>=min_scores[0])&&(sab_1<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				tmp_b2 = tmpB2[sb_2];
				if (((sab_1+sa_2+sb_2)>=min_scores[1])&&((sab_1+sa_2+sb_2)<=max_scores[1]))
				{
				  for (int sa_3 = 0; sa_3 <= MscA[2]; sa_3++)
				  {
					tmp_a3 = tmpA3[sa_3];
					for (int sb_3 = 0; sb_3 <= MscB[2]; sb_3++)
					{
					  sa = sa_1 + sa_2 + sa_3;
					  sb = sb_1 + sb_2 + sb_3;
					  if (((sa+sb)>=min_scores[2])&((sa+sb)<=max_scores[2])){
						//Rprintf("a");
						prob(sa,sb) = prob(sa,sb) + tmp_a1*tmp_b1*tmp_a2*tmp_b2*tmp_a3*tmpB3[sb_3];
					  }
					}
				  }
				}
			  }
			}
		  }
		}
	  }
	}// nMod ==3
	
	if (nMod == 4)
	{
	  for (int sa_1 = 0; sa_1 <= MscA[0]; sa_1++)
	  {
		tmp_a1 = tmpA1[sa_1];
		for (int sb_1 = 0; sb_1 <= MscB[0]; sb_1++)
		{
		  tmp_b1 = tmpB1[sb_1];
		  sab_1 = sa_1 + sb_1;
		  if ((sab_1>=min_scores[0])&&(sab_1<=max_scores[0]))
		  {
			for (int sa_2 = 0; sa_2 <= MscA[1]; sa_2++)
			{
			  tmp_a2 = tmpA2[sa_2];
			  for (int sb_2 = 0; sb_2 <= MscB[1]; sb_2++)
			  {
				tmp_b2 = tmpB2[sb_2];
				sab_2 = sa_2 + sb_2;
				if (((sab_1+sab_2)>=min_scores[1])&&((sab_1+sab_2)<=max_scores[1]))
				{
				  for (int sa_3 = 0; sa_3 <= MscA[2]; sa_3++)
				  {
					tmp_a3 = tmpA3[sa_3];
					for (int sb_3 = 0; sb_3 <= MscB[2]; sb_3++)
					{
					  tmp_b3 = tmpB3[sb_3];
					  if (((sab_1+sab_2+sa_3+sb_3)>=min_scores[2])&&((sab_1+sab_2+sa_3+sb_3)<=max_scores[2]))
					  {
						for (int sa_4 = 0; sa_4 <= MscA[3]; sa_4++)
						{
						  tmp_a4 = tmpA4[sa_4];
						  for (int sb_4 = 0; sb_4 <= MscB[3]; sb_4++)
						  {
							sa = sa_1 + sa_2 + sa_3 + sa_4;
							sb = sb_1 + sb_2 + sb_3 + sb_4;
							if (((sa+sb)>=min_scores[3])&((sa+sb)<=max_scores[3])){
							  prob(sa,sb) = prob(sa,sb) + tmp_a1*tmp_b1*tmp_a2*tmp_b2*tmp_a3*tmp_b3*tmp_a4*tmpB4[sb_4];
							}
						  }
						}
					  }
					}
				  }
				}
			  }
			}
		  }
		}
	  }
	}// nMod ==4
	
  }
  return;
}
