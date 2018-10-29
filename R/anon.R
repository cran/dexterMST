
ROUTING = list(all = 0L, last = 1L)

elsym0=function(b, a, first, last)
{
  elsym0C(as.double(b), 
          as.integer(a), 
          as.integer(first-1),
          as.integer(last-1))
}

elsym_submerge = function(g_list, range_list, routing=c('all','last'))
{
  routing = match.arg(routing)

  elsym_submergeC(lapply(g_list, as.double), 
                  lapply(range_list, as.integer),
                  ROUTING[[routing]] )

}

elsym_merge = function(b, a, first, last, g_list, range_list, mi1=c(-1,-1), mi2=c(-1,-1), routing=c('all','last'))
{
  # c-indexing
  routing = match.arg(routing)
  first = lapply(first, function(f){as.integer(f-1)})
  last  = lapply(last, function(f){as.integer(f-1)})
  elsym_mergeC(as.double(b), 
               as.integer(a), 
               as.list(first),
               as.list(last),
               lapply(g_list, as.double),
               lapply(range_list, as.integer), 
               as.integer(mi1 - 1),
               as.integer(mi2 - 1), 
               ROUTING[[routing]] )
}

E.STEP_MS<-function(b, a, booklet)
{
  first = lapply(booklet$first, function(f){as.integer(f-1)})
  last  = lapply(booklet$last, function(f){as.integer(f-1)})
  E_STEP_MSC( as.double(b),
              as.integer(a),
              as.list(first),
              as.list(last),
              as.integer(booklet$scoretab),
              as.integer(booklet$min_scores),
              as.integer(booklet$max_scores),
              ROUTING[[booklet$routing]])
}

H.STEP_MS<-function(b, a, booklet)
{
  first = lapply(booklet$first, function(f){as.integer(f-1)})
  last  = lapply(booklet$last, function(f){as.integer(f-1)})
  H_STEP_MSC( as.double(b),
              as.integer(a),
              as.list(first),
              as.list(last),
              as.integer(booklet$scoretab),
              as.integer(booklet$min_scores),
              as.integer(booklet$max_scores),
              ROUTING[[booklet$routing]])
}

############### Miscell. ##################

# Expected distribution on booklet given one ability theta
pscore_mst <- function(theta, prms, booklet_id)
{
  if (!booklet_id %in% names(prms$inputs$bkList)) stop("booklet_id in pscore_mst not found")
  first=prms$inputs$bkList[[booklet_id]]$first
  last=prms$inputs$bkList[[booklet_id]]$last
  routing = prms$inputs$bkList[[booklet_id]]$routing
  b = prms$mst_est$b
  a = prms$mst_est$item_score
  
  nMod=length(first)
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  min_scores = prms$mst_inputs$bkList[[booklet_id]]$min_scores
  max_scores = prms$mst_inputs$bkList[[booklet_id]]$max_scores
  for (m in 1:nMod)
  {
    g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
    range_list[[m]]=min_scores[m]:max_scores[m]
  }
  g=elsym_submerge(g_list,range_list,routing)
  
  p=rep(0, length(g))
  for (s in 1:length(g))
  {
    p[s]=g[s]*exp((s-1)*theta)
  }
  return(p/sum(p))
}

E_score_mst <- function(theta, prms, booklet_id)
{
  p = pscore_mst(theta, prms, booklet_id)
  E = 0
  for (s in 1:length(p)) E = E + (s-1)*p[s]
  return(E)
}



############### matrix of item-total regressions per booklet ################
# RAsch and IM
# This one for a normal booklet with no routing
ittotmatIM_MS_none<-function (b, c, a, first, last) 
{
  ms=sum(a[last])
  mm=sum((last+1)-first)
  pi=matrix(0, mm,(ms+1))
  logb = log(b)
  logc = log(c)
  for (s in 0:ms)
  {
    eta = exp(logb + (a * s) * logc)
    g = elsym0(eta, a, first, last)
    k = 1
    for (it in 1:length(first)) 
    {
      gi = elsym0(eta, a, first[-it], last[-it])
      for (j in first[it]:last[it]) 
      {
        idx = s + 1 - a[j]
        if ((idx > 0) & (idx <= length(gi))) 
        {
          pi[k, s + 1] = exp(log(eta[j]) + log(gi[idx]) - log(g[s + 1]))
        }
        k = k + 1
      }
    }
  }
  return(pi)
}

#Last routing
ittotmatIM_MS_last = function(b, c, a, first, last, min_scores, max_scores) 
{
  nMod=length(first) 
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  mm=0; ms=1
  for (m in 1:nMod)
  {
    mm=mm+sum(last[[m]]-first[[m]]+1)
    ms=ms+sum(a[last[[m]]])
  }
  pi=matrix(0, mm, ms)
  min_score=sum(min_scores)
  max_score=sum(max_scores)
  
  logb = log(b); logc = log(c)
  for (s in min_score:max_score)
  {
    eta = exp(logb + (a * s) * logc)
    for (m in 1:nMod)
    {
      g_list[[m]]=elsym0(eta, a, first[[m]], last[[m]])
      range_list[[m]]=min_scores[m]:max_scores[m]
    }
    g=elsym_submerge(g_list,range_list,"last")
    k=1
    for (m in 1:nMod)
    {
      for (it in 1:length(first[[m]])) 
      {
        mx_it=a[last[[m]][it]]
        range_list[[m]]=(min_scores[m]:max_scores[m]-mx_it)[which((min_scores[m]:max_scores[m]-mx_it)>=0)]
        g_list[[m]]=elsym0(eta, a, first[[m]][-it], last[[m]][-it]) 
        gi=elsym_submerge(g_list,range_list,"last")
        for (j in first[[m]][it]:last[[m]][it]) 
        {
          idx = s + 1 - a[j]
          if ((idx > 0) & (idx <= length(gi))&(g[s+1]>0)) 
          {
            pi[k, s + 1] = exp(log(eta[j]) + log(gi[idx]) - log(g[s + 1]))
          }
          k = k + 1
        }
      }
      range_list[[m]]=min_scores[m]:max_scores[m]
      g_list[[m]]=elsym0(eta,a,first[[m]],last[[m]])
    }
  }
  return(pi)
}

### All routing
ittotmatIM_MS_all = function(b, c, a, first, last, min_scores, max_scores) 
{
  nMod = length(first) 
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  mm=0; ms=1
  for (m in 1:nMod)
  {
    mm=mm+sum(last[[m]]-first[[m]]+1)
    ms=ms+sum(a[last[[m]]])
  }
  pi=matrix(0, mm, ms)
  min_score=0
  max_score=max_scores[nMod]
  
  logb = log(b); logc = log(c)
  for (s in min_score:max_score)
  {
    eta = exp(logb + (a * s) * logc)
    for (m in 1:nMod)
    {
      g_list[[m]]=elsym0(eta, a, first[[m]], last[[m]])
      range_list[[m]]=min_scores[m]:max_scores[m]
    }
    g=elsym_submerge(g_list,range_list,"all")
    k=1
    for (m in 1:nMod)
    {
      for (it in 1:length(first[[m]])) 
      {
        mx_it=a[last[[m]][it]]
        for (j in nMod:m){
          range_list[[j]]=(min_scores[j]:max_scores[j]-mx_it)[which((min_scores[j]:max_scores[j]-mx_it)>=0)]
        }
        g_list[[m]]=elsym0(eta, a, first[[m]][-it], last[[m]][-it]) 
        gi=elsym_submerge(g_list,range_list,"all")
        for (j in first[[m]][it]:last[[m]][it]) 
        {
          idx = s + 1 - a[j]
          if ((idx > 0)&(idx <= length(gi))&(g[s+1]>0)) 
          {
            pi[k, s + 1] = exp(log(eta[j]) + log(gi[idx]) - log(g[s + 1]))
          }
          k = k + 1
        }
      }
      range_list[[m]]=min_scores[m]:max_scores[m]
      g_list[[m]]=elsym0(eta, a, first[[m]], last[[m]])
    }
  }
  return(pi)
}

######### Only ENORM
# No routing
ittotmat_MS_none<-function (b, a, first, last) 
{
  ms=sum(a[last])
  mm=sum((last+1)-first)
  pi=matrix(0, mm,(ms+1))
  for (s in 0:ms)
  {
    g = elsym0(b, a, first, last)
    k = 1
    for (it in 1:length(first)) 
    {
      gi = elsym0(b, a, first[-it], last[-it])
      for (j in first[it]:last[it]) 
      {
        idx = s + 1 - a[j]
        if ((idx > 0) & (idx <= length(gi))) 
        {
          pi[k, s + 1] = b[j]*gi[idx]/g[s+1]
        }
        k = k + 1
      }
    }
  }
  return(pi)
}

# Last routing
ittotmat_MS_last<-function(b, a, first, last, min_scores, max_scores)
{
  nMod=length(first)
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  nrow_pi=0; ncol_pi=1
  for (m in 1:nMod)
  {
    g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
    range_list[[m]]=min_scores[m]:max_scores[m]
    ncol_pi=ncol_pi+sum(a[last[[m]]])
    nrow_pi=nrow_pi+sum(last[[m]]-first[[m]]+1)
  }
  g=elsym_submerge(g_list,range_list,"last")
  pi=matrix(0,nrow_pi,ncol_pi)
  
  k=1
  for (m in 1:nMod)
  {
    for (it in 1:length(first[[m]]))
    {
      mx_it=a[last[[m]][it]]
      range_list[[m]]=(min_scores[m]:max_scores[m]-mx_it)[which((min_scores[m]:max_scores[m]-mx_it)>=0)]
      g_list[[m]]=elsym0(b,a,first[[m]][-it],last[[m]][-it]) 
      gi=elsym_submerge(g_list,range_list,"last")
      for (j in first[[m]][it]:last[[m]][it])
      {
        for (s in (sum(min_scores)+1):sum(max_scores)) 
        {
          idx=s+1-a[j]
          if ((g[s+1]>0)&(idx>0)&(idx<=length(gi))){
            pi[k,s+1] = pi[k,s+1] + gi[idx]*b[j]/g[s+1]
          }
        }
        k=k+1
      }
    }
    range_list[[m]]=min_scores[m]:max_scores[m]
    g_list[[m]]=elsym0(b,a,first[[m]],last[[m]])
  }
  return(pi)
}

# All routing
ittotmat_MS_all<-function(b, a, first, last, min_scores, max_scores)
{
  nMod = length(first)
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  nrow_pi=0; ncol_pi=1
  for (m in 1:nMod)
  {
    g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
    range_list[[m]]=min_scores[m]:max_scores[m]
    ncol_pi=ncol_pi+sum(a[last[[m]]])
    nrow_pi=nrow_pi+sum(last[[m]]-first[[m]]+1)
  }
  g=elsym_submerge(g_list,range_list,"all")
  
  pi=matrix(0,nrow_pi,ncol_pi)
  k=1
  for (m in 1:nMod)
  {
    for (it in 1:length(first[[m]]))
    {
      mx_it=a[last[[m]][it]]
      for (j in nMod:m){
        range_list[[j]]=(min_scores[j]:max_scores[j]-mx_it)[which((min_scores[j]:max_scores[j]-mx_it)>=0)]
      }
      g_list[[m]]=elsym0(b, a, first[[m]][-it], last[[m]][-it]) 
      gi=elsym_submerge(g_list,range_list,"all")
      for (j in first[[m]][it]:last[[m]][it])
      {
        for (s in 1:max_scores[nMod]) 
        {
          idx=s+1-a[j]
          if ((g[s+1]>0)&(idx>0)){
            pi[k,s+1] = pi[k,s+1] + gi[idx]*b[j]/g[s+1]
          }
        }
        k=k+1
      }
    }
    range_list[[m]]=min_scores[m]:max_scores[m]
    g_list[[m]]=elsym0(b, a, first[[m]], last[[m]])
  }
  return(pi)
}

## This function is important because ittot_mat or all other
# that loop over modules do not return the 
# parameters in the original order.
first_last2index_MST<-function(first,last, routing="all")
{
  indx_set=NULL
  nMod=length(first)
  for (m in 1:nMod)
  {
    for (i in 1:length(first[[m]]))
    {
      indx_set=c(indx_set,first[[m]][i]:last[[m]][i])
    }
  }
  return(indx_set)
}

############### Calibration functions



# Enorm calibration of MST booklets
# @param booklets list, each element is a list representing a single booklet (= unique route through a test)
# each booklet contains
#   modules: integer vector of module indexes
#   min_scores, 
#   max_scores: integer vectors denoting the range of possible scores 
#     after each module in this booklet on basis of routing rules
#   routing: type of routing, for now always 'all'
#   items: character vector, id's of items in the booklet
#   scoretab: integer vector denoting the number of respndents to achieve each score,
#     the indexes correspond to scores, range always from 0 to the sum of the maximum scores of the items
#     (so independent of routing and independent of impossible scores in case of weird weights)
#   first,
#   last: list of integer vectors, on for each module, denoting first and last postion in the score table ordered by 
#       (item_id, item_score) without the 0 score category
# @param a: vector of item_scores arranged by item_id, item_score, also excluding the 0 score category
# @param sufI: tally of respondents achieving each item_score
# @param nIter: max number of iterations. Default is 500
# @param fixed_b: vector indicating which parameters re fixed (value) and free to estimate (=NA)
# Default value of fixed_b is NULL which means no fixed parameters
Calibrate_MST <-function(booklets, a, sufI, nIter=500, fixed_b=NULL)
{
  b=rep(1,length(sufI))
  nb=length(booklets)
  EsufI=sufI
  ref=1
  
  if (is.null(fixed_b)) # if no fixed parameters
  {
    nn=sum(sufI)
      ### Implicit equations
    converged=FALSE
    iter=0
    pb = txtProgressBar(min=0, max=nIter) 
    while ((!converged) && (iter<=nIter))
    {
      iter = iter+1
      EsufI = EsufI-EsufI
      for (bl in 1:nb) EsufI = EsufI + E.STEP_MS(b, a, booklets[[bl]])
      b = b*sufI/EsufI
      converged = ((max(abs(sufI-EsufI))/nn) < 1e-04)
      setTxtProgressBar(pb, value=iter)
    }
  
      ### Newton-Raphson 
    H=matrix(0,length(a),length(a))
    converged=FALSE
    scale=2
    while ((!converged) && (iter<=nIter))
    {
      iter=iter+1
      EsufI=EsufI-EsufI
      H=H-H
      
      for (bl in 1:nb)
      {
        EsufI = EsufI + E.STEP_MS(b, a, booklets[[bl]])
        if (length(booklets[[bl]]$items)>2){
          H     = H     + H.STEP_MS(b, a, booklets[[bl]]) 
        }
      }
      H[ref,]=0; H[,ref]=0
      H[ref,ref]=1
      EsufI[ref] = sufI[ref]
      b = b*exp(solve(H*scale,sufI-EsufI))
      #if(!all(is.finite(b))) browser()
      converged = (max(abs(EsufI-sufI))/nn<1e-10)
      
      setTxtProgressBar(pb, value=iter)
      scale = max(1, scale-1)
    }
    close(pb)
  }else # there are fixed parameters
  {
    fixed_set=which(!is.na(fixed_b))
    update_set=which(is.na(fixed_b))
    b[fixed_set]=fixed_b[fixed_set]
    nn=sum(sufI[update_set])
        ### Implicit equations
    converged=FALSE
    iter=0
    pb = txtProgressBar(min=0, max=nIter) # max van progressbar klopt neit helemaal, bij NR kan ie er overheen
    while ((!converged) && (iter<=nIter))
    {
      iter = iter+1
      EsufI = EsufI-EsufI
      for (bl in 1:nb) EsufI = EsufI = EsufI + E.STEP_MS(b, a, booklets[[bl]])
      b[update_set] = b[update_set]*sufI[update_set]/EsufI[update_set]
      converged=(max(abs(sufI[update_set]-EsufI[update_set]))/nn<1e-04)
      setTxtProgressBar(pb, value=iter)
    }
    
    ### Newton-Raphson 
    H=matrix(0,length(a),length(a))
    converged=FALSE
    scale=2
    while ((!converged) && (iter<=nIter))
    {
      iter=iter+1
      EsufI=EsufI-EsufI
      H=H-H
      for (bl in 1:nb)
      {
        EsufI = EsufI + E.STEP_MS(b, a, booklets[[bl]])
        if (length(booklets[[bl]]$items)>2){
          H     = H     + H.STEP_MS(b, a, booklets[[bl]])
        }
      }
      H[fixed_set,]=0
      H[,fixed_set]=0
      diag(H)[fixed_set]=1
      EsufI[fixed_set]=sufI[fixed_set]
      b = b*exp(solve(H*scale,sufI-EsufI))
      converged=(max(abs(EsufI[update_set]-sufI[update_set]))/nn<1e-10)
      setTxtProgressBar(pb, value=iter)
      scale = max(1, scale-1)
    }
    close(pb)
  }
  if ((!converged) && (iter=nIter)) warning(paste("Note converged in ", as.character(nIter), " iterations"))
  
  hh = toDexter(b, a, H, booklets, fixed_b = fixed_b)
  return(list(b=b, eta=-log(b), beta=hh$beta, E=EsufI, O=sufI, se.cml=sqrt(diag(hh$acov.beta)), acov.beta = hh$acov.beta))
}

# Fit Rasch and interaction Model for one booklet
# All arguments for one booklet locally:
#   min_scores, 
#   max_scores: integer vectors denoting the range of possible scores 
#     after each module in this booklet on basis of routing rules
#   routing: type of routing; 'all' or 'last'
#   scoretab: integer vector denoting the number of respndents to achieve each score,
#     the indexes correspond to scores, range always from 0 to the sum of the maximum scores of the items
#     (so independent of routing and independent of impossible scores in case of weird weights)
#   first,
#   last: list of integer vectors, on for each module, denoting first and last postion in the score table ordered by 
#       (item_id, item_score) without the 0 score category
# @param a: vector of item_scores arranged by item_id, item_score, also excluding the 0 score category
# @param sufI: tally of respondents achieving each item_score
# @param sufC: <sum(item_score * sumScore)>
# @param nIter: max number of iterations

Estim_MST <-function(a, first, last, min_scores, max_scores, sufI, sufC, scoretab, routing)
{
  nMod = length(first)
  unlist_first= unlist(first, use.names = F)
  unlist_last= unlist(last, use.names = F)
  nI=length(unlist_first)
  b=rep.int(1,length(sufI))
  EsufI=sufI
  mm=sum(scoretab)
  indx_ic=order(unlist_first)
  
  C = rep(1:nI, unlist_last-unlist_first+1)
  ic=rep.int(1,nI)
  var.ic=vector("numeric", nI)
  HRM=matrix(0,length(b),length(b))
  
  ## Rasch Model
  
  ## Implicit Equations
  converged=2
  while (converged>0.001)
  {
    if (routing=="last") pi_mat = ittotmat_MS_last(b, a, first, last, 
                                                  min_scores, max_scores)
    if (routing=="all")  pi_mat = ittotmat_MS_all(b, a, first, last, 
                                                  min_scores, max_scores)
    indx_set=first_last2index_MST(first,last)
    EsufI[indx_set]=pi_mat%*%scoretab 
    b=b*sufI/EsufI
    converged=(max(abs(sufI-EsufI))/mm)
  }
  
  ## NR per item
  converged=2
  scale=2
  while(converged>0.0001)
  {
    converged=-1
    if (routing=="last") pi_mat =ittotmat_MS_last(b, a, first, last, 
                                                  min_scores, max_scores)
    if (routing=="all")  pi_mat = ittotmat_MS_all(b, a, first, last, 
                                                  min_scores, max_scores)
    pi_mat[is.na(pi_mat)]=0
    indx_set=first_last2index_MST(first,last)
    pi_mat=pi_mat[indx_set,]
    for (m in 1:nMod)
    {
      for (i in 1:length(first[[m]]))
      {
        upd_set=first[[m]][i]:last[[m]][i]
        pi=pi_mat[upd_set,,drop=FALSE]
        E=sufI[upd_set]-pi%*%scoretab
        H=-pi%*%tcrossprod(diag(scoretab),pi) #(diag(scoretab)%*%t(pi)) 
        diag(H)=pi%*%scoretab+diag(H)
        update=solve(H*scale,E)
        b[upd_set]=b[upd_set]*exp(update)
        converged=max(converged,max(abs(E))/mm) 
        HRM[upd_set,upd_set]=H
      }
    }
    if (converged<1) scale=1
  }
  
  bRM=b
  cRM=ic
  
  ## IM
  converged=2
  scale=2
  while(converged>0.001)
  {
    converged=-1
    if (routing=="last") pi_mat = ittotmatIM_MS_last(b, ic[C], a, first, last, min_scores, max_scores) 
    if (routing=="all")  pi_mat =  ittotmatIM_MS_all(b, ic[C], a, first, last, min_scores, max_scores) 
    indx_set=first_last2index_MST(first,last)
    pi_mat=pi_mat[indx_set,]
    pi_mat[is.na(pi_mat)]=0
    tel_ic=1
    for (m in 1:nMod)
    {
      for (i in 1:length(first[[m]]))
      {
        ic_indx=indx_ic[tel_ic]
        upd_set=first[[m]][i]:last[[m]][i]
        pi=pi_mat[upd_set,,drop=FALSE]
        E=sufI[upd_set]-pi%*%scoretab
        H=-pi%*%tcrossprod(diag(scoretab),pi) 
        diag(H)=pi%*%scoretab+diag(H)
        
        ncol_pi=ncol(pi); nrow_pi=nrow(pi)
        E=c(E,sufC[ic_indx])    ## note the order!
        H=cbind(H,rep.int(0,nrow(H)))
        H=rbind(H,rep.int(0,ncol(H)))
        k=1
        e0=0; e1=0
        f=matrix(0,nrow_pi,ncol_pi)
        g=matrix(0,nrow_pi,ncol_pi)
        h=0
        for (j in upd_set)
        {
          E[length(E)]=E[length(E)]-a[j]*sum((0:(ncol_pi-1))*scoretab*pi[k,])
          e0=e0+a[j]*pi[k,]
          e1=e1+a[j]^2*pi[k,]
          f[k,]=a[j]*(0:(ncol_pi-1))*pi[k,]
          g[k,]=pi[k,]
          h=h+a[j]*(0:(ncol_pi-1))*pi[k,]
          k=k+1
        }
        H[nrow(H),nrow(H)]=sum((0:(ncol_pi-1))^2*(e1-e0^2)*scoretab)
        for (k in 1:nrow(f))
        {
          H[k,nrow(H)]=sum((f[k,]-g[k,]*h)*scoretab)
          H[nrow(H),k]=H[k,nrow(H)]
        }
        update=solve(H*scale,E)
        b[upd_set]=b[upd_set]*exp(update[-length(update)])
        ic[ic_indx]=ic[ic_indx]*exp(update[length(update)]) ## note the order
        var.ic[ic_indx]=solve(H)[nrow(H),nrow(H)] ## note the order
        tel_ic=tel_ic+1
        converged=max(converged,max(abs(E))/mm)
      }
    }
    if (converged<1) scale=1
  }
  
  return(list(routing=routing, bRM=bRM,cRM=cRM,bIM=b,cIM=ic,
              se.c=sqrt(var.ic),HRM=HRM, 
              fit.stats=log(ic)/sqrt(var.ic)))
}

## Generate response NRM

rNRM=function(theta, b, a, first, last)
{
  sampleNRM(as.double(theta),
             as.double(b), 
             as.integer(a), 
             as.integer(first-1),
             as.integer(last-1))
}


################################### Abilities

#' #' Fisher information function for each path/booklet in a test
#' #'
#' #' @param db an dextermst db handle
#' #' @param parms a parms object producted by \code{\link{fit_enorm_mst}}
#' #' @param test_id the id of a test
#' #' @param theta vector of abilities for which information is required
#' #'
#' path.information = function(db, parms, test_id, theta)
#' {
#'   bks = parms$mst_inputs$booklet_design %>%
#'     filter(test_id == test_id) %>%
#'     distinct(booklet_id) %>%
#'     pull(booklet_id)
#'   nT = length(theta)
#'   nBk = length(bks)
#'   out = matrix(0,nBk, nT)
#'   for (i in 1:nBk)
#'   {
#'     bk_id = paste0(test_id,".",bks[i])
#'     first = sort(unlist(parms$inputs$bkList[[bk_id]]$first, use.names = F))
#'     last  = sort(unlist(parms$inputs$bkList[[bk_id]]$last, use.names = F))
#'     out[i,] = dexter.IJ(parms$est$b, parms$est$a, first, last, theta)
#'   }
#'   out
#' }

#### Estimate ability
#### ML estimation of theta
# uses an implicit equations algorithm
# theta_MLE_MST <- function(prms, booklet_id)
# {
#   routing = prms$mst_inputs$bkList[[booklet_id]]$routing
#   if (routing=="none")
#   {
#     b = prms$est$b
#     a = prms$est$a
#     first = prms$inputs$ssI$first
#     last = prms$inputs$ssI$last
#     theta = dexter:::theta_MLE(b,a,first, last)
#   }else
#   {
#     a = prms$mst_est$item_score
#     nMod=length(prms$mst_inputs$bkList[[booklet_id]]$first)
#     if (routing=="all"){
#       mxs.a=prms$mst_inputs$bkList[[booklet_id]]$max_scores[nMod]
#       mns.a=prms$mst_inputs$bkList[[booklet_id]]$min_scores[nMod]
#     }
#     if (routing=="last"){
#       mns.a=sum(prms$mst_inputs$bkList[[booklet_id]]$min_scores)
#       mxs.a=sum(prms$mst_inputs$bkList[[booklet_id]]$max_scores)
#     }
#     ms.a=sum(a[unlist(prms$mst_inputs$bkList[[booklet_id]]$last, use.names = F)])
#     theta=rep(NA, ms.a+1)
#     for (s in max(1,mns.a):(min(mxs.a,ms.a)-1))
#     {
#       escore=-1
#       theta[s]=0
#       while (abs(escore-s)>1e-1)
#       {
#         escore = E_score_mst(theta[s],prms, booklet_id)
#         theta[s] = theta[s]+log(s/escore)
#       }
#     }
#     theta=c(-Inf,theta,Inf)
#   }
#   return(theta)
# }
