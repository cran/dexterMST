
########################################################
## Score-by-score table. Adapted for MST
########################################################
# @param m        a rim object produced by fit_inter (but not yet documented anywhere what that looks like)
# @param AB       list: two mutually exclusive subsets of items as indexes of m$ss$il
# @param model    Character: Indicates which model is used: "Rasch" or "IM"
# @return         A list with tbl being a score-by-score matrix of probabilities:
#                 P(X^A_+=s_a, X^B_+=s_b|X_+=s) where s=s_a+s_b
# SSTable_MS <- function(m, AB, model) {
#   if (model=="IM") {ic=m$pars$cIM; b=m$pars$bIM} else {ic=m$pars$cRM; b=m$pars$bRM}
#   a = m$inputs$ssIS$item_score
#   first = m$inputs$ssI$first
#   last = m$inputs$ssI$last
#   
#   ## Prepare use of Dexter 
#   n_last = last + 1:length(last)
#   n_first=n_last
#   n_first[1]=first[1]
#   n_first[2:length(first)]=n_last[1:(length(first)-1)]+1L
#   n_bRM=vector("numeric",n_last[length(n_last)])+1
#   n_bIM=vector("numeric",n_last[length(n_last)])+1
#   n_a=vector("numeric",n_last[length(n_last)])
#   n_bRM[-n_first]=m$pars$bRM
#   n_bIM[-n_first]=m$pars$bIM
#   n_a[-n_first]=a
#   n_item_id=NULL
#   for (i in 1:length(m$inputs$ssI$item_id)) n_item_id=c(n_item_id,rep(m$inputs$ssI$item_id[i],m$inputs$ssI$nCat[i]+1))
#   sl=data.frame(item_id=n_item_id, item_score=n_a)
#   il=data.frame(item_id=m$inputs$ssI$item_id, nCat=m$inputs$ssI$nCat+1L, first=n_first, last=n_last)
#   ss=list(sl=sl, il=il)
#   m_dexter=list(ss=ss, est=list(bRM=n_bRM, bIM=n_bIM, cRM=m$pars$cRM, cIM=m$pars$cIM))
#   out = dexter:::SSTable(m_dexter, AB, model)$tbl
#   
#   ## figure out which scores are possible
#   min_scores = m$inputs$design$module_exit_score_min
#   max_scores = m$inputs$design$module_exit_score_max
#   routing = m$inputs$routing
#   nMod=nrow(m$inputs$design)
#   nnA=rep(0,nMod)
#   nnB=rep(0,nMod)
#   indx=split(m$inputs$ssI$indx,m$inputs$ssI$module_nbr) ##  indices of items in each module
#   MscA=rep(0,nMod)
#   MscB=rep(0,nMod)
#   for (j in 1:nMod)
#   {
#     for (i in 1:length(indx[[j]]))
#     {
#       if (indx[[j]][i] %in% AB[[1]])
#       {
#         nnA[j] = nnA[j] + 1
#         MscA[j]= MscA[j] + a[last[indx[[j]][i]]]
#       }
#       if (indx[[j]][i] %in% AB[[2]])
#       {
#         nnB[j] = nnB[j] + 1
#         MscB[j] = MscB[j] + a[last[indx[[j]][i]]]
#       }
#     }
#   }
#   
#   if (routing=="all")
#   {
#     sc_rangeAB = min_scores[nMod]:max_scores[nMod]
#     minA=0; minB=0
#     maxA=0; maxB=0
#     for (j in 1:nMod)
#     {
#       maxA = maxA + min(MscA[j], max_scores[j]-maxA)
#       maxB = maxB + min(MscB[j], max_scores[j]-maxB)
#       if (maxA<min_scores[j]) minB = max(minB, min_scores[j]-maxA) 
#       if (maxB<min_scores[j]) minA = max(minA, min_scores[j]-maxB) 
#     }
#     sc_rangeA = minA:maxA 
#     sc_rangeB = minB:maxB
#   }
#   
#   if (routing=="last")
#   {
#     sc_rangeAB = sum(min_scores):sum(max_scores)
#     minA=0; minB=0
#     maxA=0; maxB=0
#     for (j in 1:nMod)
#     {
#       maxA = maxA + min(MscA[j], max_scores[j])
#       maxB = maxB + min(MscB[j], max_scores[j])
#       if (sum(MscA[j])<min_scores[j]) minB = minB + min_scores[j] - MscA[j]
#       if (sum(MscB[j])<min_scores[j]) minA = minA + min_scores[j] - MscB[j]
#     }
#     sc_rangeA=minA:maxA
#     sc_rangeB=minB:maxB
#   }
#   
#   ## Remove test-scores if needed
#   hh=setdiff(0:sum(a[last]),sc_rangeAB)
#   if (length(hh)>0)
#   {
#     combinations <- expand.grid(0:(nrow(out)-1),0:(ncol(out)-1))
#     for (s in hh)
#     {
#       inx=combinations[which(apply(combinations, 1, sum) == s),]
#       for (i in 1:nrow(inx)) out[inx[i,1]+1, inx[i,2]+1]=0
#     }
#   }
#   
#   ## Remove sub-test scores
#   adapt_subrangeB=FALSE
#   # hh = setdiff(0:(ncol(out)-1),sc_rangeB)+1
#   # if (length(hh)>0){
#   #   out[,hh] = 0
#   #   adapt_subrangeB =TRUE
#   # }
#   adapt_subrangeA=FALSE
#   hh = setdiff(0:(nrow(out)-1),sc_rangeA)+1
#   if (length(hh)>0){
#     out[hh,] = 0
#     adapt_subrangeA = TRUE
#   } 
#   
#   ## Make sure the remaining probabilities sum to one
#   if ((adapt_subrangeA)|(adapt_subrangeB))
#   {
#     for (s in sc_rangeAB)
#     {
#       inx=combinations[which(apply(combinations, 1, sum) == s),]
#       sm=0
#       for (i in 1:nrow(inx)) sm = sm + ifelse(!is.na(out[inx[i,1]+1, inx[i,2]+1]), out[inx[i,1]+1, inx[i,2]+1], 0) 
#       for (i in 1:nrow(inx)) out[inx[i,1]+1, inx[i,2]+1] = out[inx[i,1]+1, inx[i,2]+1]/sm
#     }
#   }
#   
#   return(list(tbl=out,m=m,AB=AB,model=model))
# }


## Calculates E[S_a|s]
E_profile_MS_enorm <- function(m, A, booklet_id)
{
  nSub=length(A)
  Msc = sum(pull(m$inputs$ssIS, 'item_score')[pull(m$inputs$ssI, 'last')[unlist(A)]])

  AA=vector("list",2)
  E_RM = matrix(0, nSub, Msc+1)
  for (j in 1:nSub)
  {
    AA[[1]] = A[[j]]
    AA[[2]] = unlist(A[setdiff(1:nSub,j)], use.names = FALSE)
    E_RM[j,] = SSTable_MS_enorm(m,AA,booklet_id)$E_A
  }
  return(E_RM)
}


# E_profile_MS <- function(m, A)
# {
#   nSub=length(A)
#   Msc = sum(m$inputs$ssIS$item_score[m$inputs$ssI$last]) # TO DO: adapt when first last are passed as lists
#   Msc_sub = rep(0,nSub)
#   
#   AA=vector("list",2)
#   E_RM = matrix(0, nSub, Msc+1)
#   E_IM = matrix(0, nSub, Msc+1)
#   for (j in 1:nSub)
#   {
#     AA[[1]]=A[[j]]
#     AA[[2]]=unlist(A[setdiff(1:nSub,j)], use.names = FALSE)
#     hh_RM = SSTable_MS(m,AA,"RM")$tbl
#     hh_IM = SSTable_MS(m,AA,"IM")$tbl
#     Msc_sub[j] = nrow(hh_RM)-1
#     for (i in 1:nrow(hh_RM))
#     {
#       sA = i-1
#       for (h in 1:ncol(hh_RM))
#       {
#         s = i + h - 1
#         E_RM[j,s] = E_RM[j,s] + sA*hh_RM[i,h]
#         E_IM[j,s] = E_IM[j,s] + sA*hh_IM[i,h] 
#       }
#     }
#   }
#   return(list(E_RM=E_RM, E_IM=E_IM))
# }


SSTable_MS_enorm <- function(prms, AB, booklet_id) 
{
  rn = 1L:nrow(prms$inputs$ssI)
  first = prms$mst_inputs$bkList[[booklet_id]]$first
  last  = prms$mst_inputs$bkList[[booklet_id]]$last
  b = prms$mst_est$b
  a = prms$mst_inputs$ssIS$item_score
  min_scores = prms$mst_inputs$bkList[[booklet_id]]$min_scores
  max_scores = prms$mst_inputs$bkList[[booklet_id]]$max_scores
  nMod=length(min_scores)
  
  ## bookkeeping
  indx=first
  rn = 1L:nrow(prms$inputs$ssI)
  for (i in 1:nMod) indx[[i]] = rn[match(indx[[i]],prms$mst_inputs$ssI$first)]
  MscA=rep(0,nMod); MscB=rep(0,nMod)
  firstA = vector("list", nMod); lastA = vector("list", nMod)
  firstB = vector("list", nMod); lastB = vector("list", nMod)
  for (i in 1:nMod)
  {
    indx_A = NULL; indx_B = NULL
    for (j in 1:length(indx[[i]]))
    {
      if (indx[[i]][j]%in%AB[[1]]) indx_A =c(indx_A, j)
      if (indx[[i]][j]%in%AB[[2]]) indx_B =c(indx_B, j)
    }
    firstA[[i]] = first[[i]][indx_A]
    lastA[[i]] = last[[i]][indx_A]
    firstB[[i]] = first[[i]][indx_B]
    lastB[[i]] = last[[i]][indx_B]
    MscA[i] = sum(a[lastA[[i]]])
    MscB[i] = sum(a[lastB[[i]]])
  }
  
  routing = prms$mst_inputs$bkList[[booklet_id]]$routing
  ## make log-elsym
  gA = vector("list", nMod)
  gB = vector("list", nMod)
  g_list=vector(mode='list', length=nMod)
  range_list=vector(mode='list', length=nMod)
  for (m in 1:nMod)
  {
    g_list[[m]]     = elsym0(b, a, first[[m]], last[[m]])
    range_list[[m]] = min_scores[m]:max_scores[m]
    if (length(firstA[[m]])>0){
      gA[[m]] = elsym0(b, a, firstA[[m]], lastA[[m]])
    }else gA[[m]] = 1
    if (length(firstB[[m]])>0){
      gB[[m]] = elsym0(b, a, firstB[[m]], lastB[[m]])
    }else gB[[m]] = 1
  }
  g = elsym_submerge(g_list,range_list,routing)

  prob = prof_prob(min_scores, max_scores, gA, gB, MscA, MscB, routing)

  CC = outer(0:sum(MscA),0:sum(MscB), FUN = "+")
  Ms = sum(MscA)+sum(MscB)
  for (s in 0:Ms)
  {
      indx_s = which(CC==s, arr.ind=T)
      if (g[s+1]>0){
        prob[indx_s] = prob[indx_s]/g[s+1]
      }else prob[indx_s] = NA
  }
  
  PA = vector("list", Ms+1)
  for (s in 0:Ms){
    indx_s = which(CC==s, arr.ind=T)
    PA[[s+1]] = rbind(indx_s[,1]-1, prob[indx_s]) 
  }
  E_A = unlist(lapply(PA, function(x){sum(x[1,]*x[2,])}), use.names = FALSE)
  
  return(list(table=prob, E_A=E_A))
}

prof_prob = function(min_scores, max_scores, gA, gB, MscA, MscB, routing)
{
  prob =  matrix(0, sum(MscA)+1, sum(MscB)+1 )
  nMod = length(min_scores)
  
  if (nMod>4) stop("profiles not (yet) implemented for more than 4 modules")
  
  prof_probC(prob, 
             as.integer(min_scores), as.integer(max_scores),
             gA, gB,
             as.integer(MscA), as.integer(MscB),
             as.integer(nMod), ROUTING[[routing]])
  prob
}


##########################################
#' Profile analysis
#'
#' Expected and observed domain scores per booklet and test score
#'
#' @param parms An object returned by \code{\link{fit_enorm_mst}}
#' @param item_property the name of the item property used to define the domains.
#' @param domains data.frame with column item_id and a column whose name matches `item_property` 
#' @param tests vector of 1 or more test_id's to limit the output. If NULL, profiles are computed for all tests.
#' 
profile_tables_mst = function(parms, domains, item_property, tests=NULL)
{
  if(!item_property %in% colnames(domains))
    stop(paste('column', item_property, 'not found in domains'))
  
  if(is.double(domains[[item_property]]))
  {
    if(any(domains[[item_property]] %% 1 > 0))
      stop(paste(item_property, 'should be categorical (character or integer), not double'))
    domains[[item_property]] = as.character(domains[[item_property]])
    ip_transform = as.integer
  } else
  {
    ip_transform = as.character
  }
  
  
  domains = parms$inputs$ssI %>%
    mutate(rn = row_number()) %>%
    inner_join(domains,by='item_id') %>%
    mutate(dcat = dense_rank(.data[[!!item_property]]))
  
  A = split(domains$rn, domains$dcat)
  
  lapply(parms$mst_inputs$bkList, 
    function(bk)
    {
      if(is.null(tests) || bk$test_id %in% tests)
      {
        relevant = domains %>%
          semi_join(tibble(item_id = bk$items), by='item_id') 
        
        a = lapply(A, intersect, y = pull(relevant,'rn'))
        a = a[unlist(lapply(a, function(v){length(v)>0}))] 
        
        prop = distinct(relevant, .data$dcat, .data[[!!item_property]]) %>% 
          arrange(.data$dcat) 
        # hier wordt item_property data type vermangeld, iets aan doen
        p = E_profile_MS_enorm(m = parms, A = a, bk$booklet_id) #!
        rownames(p) = prop[[item_property]]
        
        as_tibble(t(p)) %>%
          add_column(test_id=bk$test_id,
                      booklet_id=gsub(paste0('^',bk$test_id,'\\.'),'',bk$booklet_id,perl=TRUE),
                      sumScore=0:(ncol(p)-1),
                     .before=1) %>%
          gather(key=!!item_property, value='expected_domain_score',-.data$test_id, -.data$booklet_id, -.data$sumScore)
      } else
      {
        NULL
      }
    }) %>%
    bind_rows() %>%
    mutate(!!item_property := ip_transform(.data[[!!item_property]])) %>%
    as.data.frame()
}

