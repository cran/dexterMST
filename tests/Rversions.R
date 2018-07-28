rversions = function()
{
  ###########################################################
  # R-versions of vital functions for didactic purposes
  # One may choose to replace elsym_merge_R by elsym_merge
  ############################################################
  ## Elsym
  elsymRM_R<-function(b)
  {
    N=length(b)
    g=matrix(0,N+1)
    g[1]=1; g[2]=b[1]
    for (j in 2:N)
    {
      for (s in (j+1):2) g[s]=g[s]+g[s-1]*b[j]
    }
    return(g)
  }
  
  ESF_merge_last_R<-function(esf1,range1,esf2,range2)
  {
    out = array(0,dim=length(esf1)+length(esf2)-1)
    for (i in range1)
    {
      for (j in range2)
      {
        out[i+j+1] = out[i+j+1]+esf1[i+1]*esf2[j+1]
      }
    }
    return(out)
  }
  
  ESF_merge_all_R<-function(esf1,range1,esf2,range12)
  {
    out= array(0,dim=length(esf1)+length(esf2)-1)
    for (i in (range1+1))
    {
      for (j in (range12+1))
      {
        if ( ((j-i)>=0) & ((j-i)<length(esf2)) )
        {
          out[j] = out[j]+esf1[i]*esf2[j-i+1]
        }
      }
    }
    return(out)
  }
  
  elsym_submerge_R<-function(g_list, range_list, routing=c("all","last"))
  {
    routing <- match.arg(routing)
    g=g_list[[1]]
    n_g=length(g_list)
    if (n_g>1)
    {
      range_1=0
      range_2=range_list[[1]]
      if (routing=="last")
      {
        for (m in 2:n_g)
        {
          range_1=(min(range_1)+min(range_2)):(max(range_1)+max(range_2))
          range_2=range_list[[m]]
          g=ESF_merge_last(g,range_1,g_list[[m]],range_2)
        }
      }
      if (routing=="all")
      {
        for (m in 2:n_g)
        {
          range_1=min(range_2):max(range_2) 
          range_2=range_list[[m]]
          g=ESF_merge_all(g,range_1,g_list[[m]],range_2)
        }
      }
    }
    return(g)
  }
  
  
  elsym_merge_R <- function(b, a, first, last, g_list, range_list, mi1=c(-1,-1), mi2=c(-1,-1), routing)
  {
    nMod=length(first)
    if ((mi1[2]<0)&(mi2[2]<0)){
      g=elsym_submerge(g_list,range_list,routing)
    }else
    {
      if (mi1[2]*mi2[2]<=0)  # one item out
      {
        if (mi1[2]>0){
          m=mi1[1]; item=mi1[2]
        }else
        {
          m=mi2[1]; item=mi2[2]
        }
        mx_it=a[last[[m]][item]]
        if (routing=="all")
        {
          for (j in nMod:m){
            range_list[[j]]=(range_list[[j]]-mx_it)[which((range_list[[j]]-mx_it)>=0)]
          }
        }
        if (routing=="last")
        {
          range_list[[m]]=(range_list[[m]]-mx_it)[which((range_list[[m]]-mx_it)>=0)]
        }
        g_list[[m]]=elsym0(b, a, first[[m]][-item], last[[m]][-item]) 
        g=elsym_submerge(g_list,range_list,routing)
      }else # two items out
      {
        if (mi1[1]==mi2[1]) # same module
        {
          m=mi1[1]
          mx_it=a[last[[m]][mi1[2]]]+a[last[[m]][mi2[2]]]
          if (routing=="all")
          {
            for (j in nMod:m){
              range_list[[j]]=(range_list[[j]]-mx_it)[which((range_list[[j]]-mx_it)>=0)]
            }
          }
          if (routing=="last")
          {
            range_list[[m]]=(range_list[[m]]-mx_it)[which((range_list[[m]]-mx_it)>=0)]
          }
          g_list[[m]]=elsym0(b, a, first[[m]][-c(mi1[2],mi2[2])], last[[m]][-c(mi1[2],mi2[2])])
        }else # different modules
        {
          # first item in first module
          m=mi1[1]; item=mi1[2]
          mx_it=a[last[[m]][item]]
          if (routing=="all")
          {
            for (j in nMod:m){
              range_list[[j]]=(range_list[[j]]-mx_it)[which((range_list[[j]]-mx_it)>=0)]
            }
          }
          if (routing=="last")
          {
            range_list[[m]]=(range_list[[m]]-mx_it)[which((range_list[[m]]-mx_it)>=0)]
          }
          g_list[[m]]=elsym0(b, a, first[[m]][-item], last[[m]][-item])
          # Second item in second module
          m=mi2[1]; item=mi2[2]
          mx_it=a[last[[m]][item]]
          if (routing=="all")
          {
            for (j in nMod:m){
              range_list[[j]]=(range_list[[j]]-mx_it)[which((range_list[[j]]-mx_it)>=0)]
            }
          }
          if (routing=="last")
          {
            range_list[[m]]=(range_list[[m]]-mx_it)[which((range_list[[m]]-mx_it)>=0)]
          }
          g_list[[m]]=elsym0(b, a, first[[m]][-item], last[[m]][-item])
        }
        g=elsym_submerge(g_list, range_list, routing)
      }
    }
    return(g)
  }
  
  
  E_STEP_MS2_R<-function(b, a, booklet) 
  {
    expect=rep(0,length=length(b))
    if (booklet$routing=="last") pi = ittotmat_MS_last(b, a, booklet$first, booklet$last, 
                                                       booklet$min_scores, booklet$max_scores)
    if (booklet$routing=="all")  pi = ittotmat_MS_all(b, a, booklet$first, booklet$last, 
                                                      booklet$min_scores, booklet$max_scores)
    indx_set=first_last2index_MST(booklet$first, booklet$last, booklet$routing)
    expect[indx_set]=expect[indx_set]+pi%*%booklet$scoretab
    return(expect)
  }
  
  E_STEP_MS_R<-function(b, a, booklet)
  {
    first=booklet$first
    last=booklet$last
    scoretab=booklet$scoretab 
    
    E=rep(0,length(b))
    nMod=length(first)
    g_list=vector(mode='list', length=nMod)
    range_list=vector(mode='list', length=nMod)
    ms=0
    for (m in 1:nMod)
    {
      g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
      range_list[[m]]=booklet$min_scores[m]:booklet$max_scores[m]
      ms=ms+sum(a[last[[m]]])
    }
    g=elsym_submerge(g_list,range_list,booklet$routing)
    
    for (m in 1:nMod)
    {
      nI=length(first[[m]])
      for (item in 1:nI)
      {
        gi=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(m,item), mi2=c(-1,-1), booklet$routing)
        for (j in first[[m]][item]:last[[m]][item])
        {
          for (s in (a[j]+1):ms)
          {
            if (g[s]>0) E[j]=E[j]+scoretab[s]*gi[s-a[j]]*b[j]/g[s]
          }
        }
      }
    }
    return(E)
  }
  
  H.STEP_MS_R<-function(b, a, booklet)
  {
    first=booklet$first
    last=booklet$last
    scoretab=booklet$scoretab 
    routing=booklet$routing
    
    nMod<<-length(first)
    ms=length(scoretab)-1
    H=matrix(0,length(a),length(a))
    
    g_list=vector(mode='list', length=nMod)
    range_list=vector(mode='list', length=nMod)
    for (m in 1:nMod)
    {
      g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
      range_list[[m]]=booklet$min_scores[m]:booklet$max_scores[m]
    }
    g=elsym_submerge(g_list,range_list,routing)
    ms=min(ms,max(which(g>0)))
    
    for (m in 1:nMod)
    {
      nI=length(first[[m]])
      for (item in 1:nI)
      {
        gi=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(m,item), mi2=c(-1,-1), routing)
        for (j in first[[m]][item]:last[[m]][item])
        {
          for (s in (a[j]+1):ms)
          {
            if (g[s]>0)
            {
              H[j,j] = H[j,j]+scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(1-(gi[s-a[j]]*b[j]/g[s]))
            }
          }
          ### between categories of the same item
          if ((j+1)<=last[[m]][item])
          {
            for (k in (j+1):last[[m]][item])
            {
              for (s in (a[k]+1):ms)
              {
                if (g[s]>0)
                {
                  H[k,j] = H[k,j]-scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gi[s-a[k]]*b[k]/g[s]);
                }
              }
              H[j,k]=H[k,j]
            }
          }
          ## Between items in same module
          if ((item+1)<=nI)
          {
            for (k in (item+1):nI)
            {
              gk=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(-1,-1), mi2=c(m,k), routing)
              gik=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(m,item), mi2=c(m,k), routing)
              for (l in first[[m]][k]:last[[m]][k])
              {
                for (s in 1:ms)
                {
                  if (g[s]>0)
                  {
                    if ((s>(a[j]+a[l]))&((s-a[j]-a[l])<=length(gik))){
                      H[l,j] = H[l,j] + scoretab[s]*(gik[s-a[j]-a[l]])*((b[j]*b[l])/g[s]) 
                    }
                    if ((s>a[j])&(s>a[l])) {
                      H[l,j] = H[l,j] - scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gk[s-a[l]]*b[l]/g[s])
                    }
                  }
                }
                H[j,l]=H[l,j]
              }
            }
          }
          
          ## between items in other modules
          if ((m+1)<=nMod)
          {
            for (m2 in (m+1):nMod)
            {
              for (k in 1:length(first[[m2]]))
              {
                gk=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(-1,-1), mi2=c(m2,k), routing)
                gik=elsym_merge_R(b, a, first, last, g_list, range_list, mi1=c(m,item), mi2=c(m2,k), routing)
                for (l in first[[m2]][k]:last[[m2]][k])
                {
                  for (s in 1:ms)
                  {
                    if (g[s]>0)
                    {
                      if ((s>(a[j]+a[l]))&((s-a[j]-a[l])<=length(gik))){
                        H[l,j] = H[l,j] + scoretab[s]*(gik[s-a[j]-a[l]])*((b[j]*b[l])/g[s]) 
                      }
                      if ((s>a[j])&(s>a[l])) {
                        H[l,j] = H[l,j] - scoretab[s]*(gi[s-a[j]]*b[j]/g[s])*(gk[s-a[l]]*b[l]/g[s])
                      }
                    }
                  }
                  H[j,l]=H[l,j]
                }
              }
            }
          }
        } #END j loop
      } #END item loop
    } #END module loop
    return(H)
  }
}
